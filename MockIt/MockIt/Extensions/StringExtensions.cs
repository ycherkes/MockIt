namespace MockIt.Extensions
{
    public static class StringExtensions
    {
        public static string FirstCharToUpperCase(this string input)
        {
            return char.ToUpper(input[0]) + input.Substring(1);
        }

        public static string FirstCharToLowerCase(this string input)
        {
            return char.ToLower(input[0]) + input.Substring(1);
        }
    }
}
