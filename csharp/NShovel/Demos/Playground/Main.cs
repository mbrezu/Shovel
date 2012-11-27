using System;

namespace Playground
{
    class MainClass
    {
        public static void Main (string[] args)
        {
            Console.WriteLine (
              Shovel.Api.TestRunVm(
                Shovel.Api.MakeSources("test.sho", "'hello, world'")));
        }
    }
}
