namespace MockIt
{
    public static class StringExtensions
    {
        public static string FirstCharToUpperCase(this string input)
        {
            return char.ToUpper(input[0]) + input.Substring(1);
        }
    }
}
