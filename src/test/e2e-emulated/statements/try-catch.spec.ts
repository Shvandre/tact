import { toNano } from "@ton/core";
import type { SandboxContract, TreasuryContract } from "@ton/sandbox";
import { Blockchain } from "@ton/sandbox";
import { TryCatchTester } from "./output/try-catch_TryCatchTester";
import "@ton/test-utils";

describe("try-catch", () => {
    let blockchain: Blockchain;
    let treasury: SandboxContract<TreasuryContract>;
    let contract: SandboxContract<TryCatchTester>;

    beforeEach(async () => {
        blockchain = await Blockchain.create();
        blockchain.verbosity.print = false;
        treasury = await blockchain.treasury("treasury");

        contract = blockchain.openContract(await TryCatchTester.fromInit());

        const deployResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            null,
        );

        expect(deployResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            deploy: true,
        });
    });

    it("should implement try-catch statements correctly", async () => {
        // Check try-catch method results
        expect(await contract.getTestTryCatch1()).toEqual(7n);
        expect(await contract.getTestTryCatch2()).toEqual(101n);
        expect(await contract.getTestTryCatch3()).toEqual(4n);

        // Check state rollbacks
        expect(await contract.getGetCounter()).toEqual(0n);
        let sendResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            "increment",
        );
        expect(sendResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
        });
        expect(await contract.getGetCounter()).toEqual(1n);

        sendResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            "incrementTryCatch",
        );
        expect(sendResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
        });
        expect(await contract.getGetCounter()).toEqual(1n); // Counter should not change

        sendResult = await contract.send(
            treasury.getSender(),
            { value: toNano("10") },
            "tryCatchRegisters",
        );
        expect(sendResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
        });
        expect(await contract.getGetCounter()).toEqual(2n); // Counter should increment
    });

    it("should restore state changes if a throw occurs inside a function with non-trivial branching", async () => {
        expect(await contract.getTestTryCatch4()).toStrictEqual(true);
    });
});
