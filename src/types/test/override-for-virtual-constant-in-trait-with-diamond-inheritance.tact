primitive Int;

trait BaseTrait {}

trait T {
    virtual const BAR: Int = 100;
}

trait T2 with T {
    virtual const BAZ: Int = 100;
}

trait T3 with T {
    override const BAR: Int = 100;
}

trait Test with T2, T3 {
    override const BAR: Int = 100;
}
