import { toNano } from "@ton/core";
import type { SandboxContract, TreasuryContract } from "@ton/sandbox";
import { Blockchain } from "@ton/sandbox";
import { TraitsConstantContract } from "./output/base-trait-constant-override-2_TraitsConstantContract";
import "@ton/test-utils";

describe("base-trait-constant-override-2", () => {
    let blockchain: Blockchain;
    let treasury: SandboxContract<TreasuryContract>;
    let contract: SandboxContract<TraitsConstantContract>;

    beforeEach(async () => {
        blockchain = await Blockchain.create();
        treasury = await blockchain.treasury("treasury");

        contract = blockchain.openContract(
            await TraitsConstantContract.fromInit(),
        );

        const deployResult = await contract.send(
            treasury.getSender(),
            { value: toNano("0.5") },
            null,
        );

        expect(deployResult.transactions).toHaveTransaction({
            from: treasury.address,
            to: contract.address,
            success: true,
            deploy: true,
        });
    });

    it("should override constant correctly", async () => {
        expect(await contract.getConstant()).toEqual(10000n);
    });
});
