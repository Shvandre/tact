import "@stdlib/deploy";
import "./delayed-upgradeable";

message Add {
    amount: Int as uint32;
}

contract SampleDelayedUpgradeContract with DelayedUpgradable {
    _version: Int as uint32 = 0;
    initiatedAt: Int = 0;
    upgradeInfo: Upgrade = Upgrade{};

    owner: Address;
    counter: Int as uint32;

    init(owner: Address) {
        self.owner = owner;
        self.counter = 0;
    }

    receive() {}

    fun add(v: Int) {
        let ctx: Context = context();
        require(ctx.sender == self.owner, "Invalid sender");

        self.counter += v;
    }

    receive("increment") {
        self.add(1);
    }

    get fun counter(): Int {
        return self.counter;
    }

    get fun initiatedAt(): Int {
        return self.initiatedAt;
    }
}
