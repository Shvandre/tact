import { toNano } from "@ton/core";
import type { SandboxContract, TreasuryContract } from "@ton/sandbox";
import { Blockchain } from "@ton/sandbox";
import { TextMessageReceivers } from "./output/text-message-receivers_TextMessageReceivers";
import "@ton/test-utils";

describe("text-message-receivers", () => {
    let blockchain: Blockchain;
    let treasury: SandboxContract<TreasuryContract>;
    let contract: SandboxContract<TextMessageReceivers>;

    beforeEach(async () => {
        blockchain = await Blockchain.create();
        blockchain.verbosity.print = false;
        treasury = await blockchain.treasury("treasury");

        contract = blockchain.openContract(
            await TextMessageReceivers.fromInit(),
        );
    });

    it("should deploy", async () => {
        // Deploy the contract
        const deployResult = await contract.send(
            treasury.getSender(),
            { value: toNano("1") },
            { $$type: "Deploy", queryId: 0n },
        );

        expect(deployResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            deploy: true,
        });

        // Verify initial state
        expect(await contract.getGetCounter()).toBe(0n);
    });

    it("should increment counter with different text messages", async () => {
        // Deploy the contract
        const deployResult = await contract.send(
            treasury.getSender(),
            { value: toNano("1") },
            { $$type: "Deploy", queryId: 0n },
        );
        expect(deployResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            deploy: true,
        });

        // Verify initial state
        expect(await contract.getGetCounter()).toBe(0n);

        const sendMessage = async (
            message: Parameters<typeof contract.send>[2],
        ) => {
            const incrementResult1 = await contract.send(
                treasury.getSender(),
                { value: toNano("1") },
                message,
            );
            expect(incrementResult1.transactions).toHaveTransaction({
                from: treasury.address,
                to: contract.address,
                success: true,
            });
        };

        // Increment counter
        await sendMessage("increment'");
        expect(await contract.getGetCounter()).toBe(1n);

        await sendMessage('increment-2"');
        expect(await contract.getGetCounter()).toBe(3n);

        await sendMessage("increment-3`");
        expect(await contract.getGetCounter()).toBe(6n);

        await sendMessage("\\increment-4\\");
        expect(await contract.getGetCounter()).toBe(10n);

        await sendMessage('test \n \t \r \b \f " \\ \v \\\\ \u{4242} \xA9');
        expect(await contract.getGetCounter()).toBe(15n);
    });
});
