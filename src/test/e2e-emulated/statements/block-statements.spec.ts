import { toNano } from "@ton/core";
import type { SandboxContract, TreasuryContract } from "@ton/sandbox";
import { Blockchain } from "@ton/sandbox";
import { Test } from "./output/block-statements_Test";
import "@ton/test-utils";

describe("block-statements", () => {
    let blockchain: Blockchain;
    let treasury: SandboxContract<TreasuryContract>;
    let contract: SandboxContract<Test>;

    beforeEach(async () => {
        blockchain = await Blockchain.create();
        treasury = await blockchain.treasury("treasury");

        contract = blockchain.openContract(await Test.fromInit());

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

    it("should work correctly with block statements", async () => {
        expect(await contract.getA()).toEqual(84n);
    });

    it("should work correctly with variables from subsequent block statements", async () => {
        expect(await contract.getB()).toEqual(1308n);
    });

    it("should work correctly with variables of different types from subsequent block statements", async () => {
        expect(await contract.getC()).toEqual(557n);
    });

    it("should work correctly with variables of different types from subsequent block statements inside interpreter", async () => {
        expect(await contract.getD()).toEqual(1308n);
    });

    it("should work correctly with variables of different types from nested block statements inside interpreter", async () => {
        expect(await contract.getE()).toEqual(84n);
    });
});
