namespace ConsoleApplication2
{
    public interface IFactoryClass
    {
        IClass2<T> GetClass2<T> ();
        IClass3<T> GetClass3<T>();
    }
}
