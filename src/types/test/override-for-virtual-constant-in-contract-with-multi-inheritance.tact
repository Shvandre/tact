primitive Int;

trait BaseTrait {}
trait T {}

trait T2 {
    virtual const BAR: Int = 100;
}

contract Test with T, T2 {
    override const BAR: Int = 100;
}
