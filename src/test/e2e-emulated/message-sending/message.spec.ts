import { toNano, beginCell } from "@ton/core";
import type { Cell } from "@ton/core";
import type { SandboxContract, TreasuryContract } from "@ton/sandbox";
import { Blockchain } from "@ton/sandbox";
import { MessageTester } from "./output/message_MessageTester";
import "@ton/test-utils";

describe("message", () => {
    let blockchain: Blockchain;
    let treasury: SandboxContract<TreasuryContract>;
    let contract: SandboxContract<MessageTester>;

    beforeEach(async () => {
        blockchain = await Blockchain.create();
        blockchain.verbosity.print = false;
        treasury = await blockchain.treasury("treasury");

        contract = blockchain.openContract(await MessageTester.fromInit());

        const deployResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            { $$type: "Deploy", queryId: 0n },
        );

        expect(deployResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            deploy: true,
        });
    });

    it("should send reply correctly", async () => {
        const sendResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            "Hello",
        );

        expect(sendResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            body: beginCell()
                .storeUint(0, 32)
                .storeStringTail("Hello")
                .endCell(),
        });
    });

    it("should bounce on unknown message", async () => {
        const sendResult = await treasury.send({
            to: contract.address,
            value: toNano("10"),
            body: beginCell().storeStringTail("Unknown").endCell(),
        });

        expect(sendResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: false,
            exitCode: 130,
        });
    });

    it("should send with intermediate reservations", async () => {
        // emit, nativeReserve, send
        let balanceBefore = (await blockchain.getContract(contract.address))
            .balance;
        await expectMessageFromToWithDefaults({
            treasury,
            contract,
            body: textMsg("ReserveAtMost_1"),
        });
        let balanceAfter = (await blockchain.getContract(contract.address))
            .balance;
        // The difference is at most 0.05 Toncoin reserved on top of the previous balance
        expect(balanceAfter - balanceBefore <= toNano("0.05")).toBe(true);

        // send, nativeReserve, send
        balanceBefore = (await blockchain.getContract(contract.address))
            .balance;
        await expectMessageFromToWithDefaults({
            treasury,
            contract,
            body: textMsg("ReserveAtMost_2"),
        });
        balanceAfter = (await blockchain.getContract(contract.address)).balance;
        // The difference is at most 0.05 Toncoin reserved on top of the previous balance
        expect(balanceAfter - balanceBefore <= toNano("0.05")).toBe(true);
    });
});

/**
 * A helper function to send a message `body` from the `treasury` to the `contract`
 * with specified `value` and `bounce` values, and then expect that transaction
 * to be successful or not (`success`), and if not — expect a certain exit code from it
 */
async function expectMessageFromTo(args: {
    treasury: SandboxContract<TreasuryContract>;
    contract: SandboxContract<MessageTester>;
    body: Cell | null;
    value: bigint;
    bounce: boolean;
    success: boolean;
    exitCode: number;
}) {
    const sendResult = await args.treasury.send({
        to: args.contract.address,
        value: args.value,
        bounce: args.bounce,
        body: args.body,
    });
    expect(sendResult.transactions).toHaveTransaction({
        from: args.treasury.address,
        to: args.contract.address,
        success: args.success,
        exitCode: args.exitCode,
    });
}

/**
 * Like `expectMessageFromTo`, but with common defaults set:
 * * value: `toNano("10")`
 * * bounce: `false`
 * * success: `true`
 * * exitCode: `0`
 */
async function expectMessageFromToWithDefaults(args: {
    treasury: SandboxContract<TreasuryContract>;
    contract: SandboxContract<MessageTester>;
    body: Cell | null;
}) {
    await expectMessageFromTo({
        treasury: args.treasury,
        contract: args.contract,
        body: args.body,
        value: toNano("10"),
        bounce: false,
        success: true,
        exitCode: 0,
    });
}

/** Creates a Cell message body from the passed `src` string */
function textMsg(src: string): Cell {
    return beginCell().storeUint(0, 32).storeStringTail(src).endCell();
}
