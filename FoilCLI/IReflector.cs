using System;
using System.IO;

namespace  com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReflector.
	/// </summary>
	public interface IReflector
	{
		void processMessages(TextReader ins,TextWriter outs);
	}
}
