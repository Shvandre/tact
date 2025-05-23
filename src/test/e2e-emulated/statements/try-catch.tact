fun throwWithElseIf(op: Int) {
    if (op == 1) { return }
    else if (op == 2) { return }
    else { throw(1042) }
}

contract TryCatchTester {
    counter: Int = 0;

    receive() {}

    receive("increment") {
        self.counter += 1;
    }

    receive("incrementTryCatch") {
        try {
            self.counter += 1;
            throw(123);
        }
    }

    receive("tryCatchRegisters") {
        self.counter += 1;
        self.reply("hello world 1".asComment());
        try {
            self.counter += 1;
            self.reply("hello world 2".asComment());
            emit("Something".asComment());
            throw(123);
        }
    }

    get fun getCounter(): Int {
        return self.counter;
    }

    get fun testTryCatch1(): Int {
        try {
            throwUnless(101, false);
            return 42;
        }
        return 7;
    }

    get fun testTryCatch2(): Int {
        try {
            throwIf(101, true);
            return 42;
        } catch (e) {
            return e;
        }
    }

    get fun testTryCatch3(): Int {
        try {
            let xs: Slice = beginCell().storeUint(0, 1).endCell().beginParse();
            let x: Int = xs.loadUint(1);
            try {
                throwUnless(101, false);
            } catch (e) {
                return e / x; // division by zero, exit code = 4
            }
            let e: Int = 123;
            return e;
        } catch (e) {
            return e;
        }
    }

    // bug fixed in FunC v0.4.6
    get fun testTryCatch4(): Bool {
        let x = 10;
        try {
            x = 20;
            throwWithElseIf(42);
        } catch(_) {
            return true; // x == 10;
        }
        return false;
    }
}
