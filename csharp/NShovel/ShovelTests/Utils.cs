using System;
using NUnit.Framework;
using System.Collections.Generic;

namespace ShovelTests
{
	public static class Utils
	{
		public static void ExpectException<T> (Action action, Action<T> check)
            where T: class
		{
			try {
				action ();
				Assert.Fail ();
			} catch (Exception ex) {
				check (ex as T);
			}
		}

		public static List<Shovel.SourceFile> MakeSources (params string[] namesAndContents)
		{
			List<Shovel.SourceFile> result = new List<Shovel.SourceFile> ();
			for (var i = 0; i < namesAndContents.Length; i+=2) {
				result.Add (new Shovel.SourceFile () {
					FileName = namesAndContents[i],
					Content = namesAndContents[i+1]
				}
				);
			}
			return result;
		}

	}
}

