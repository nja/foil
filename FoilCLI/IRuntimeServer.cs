using System;
using System.IO;

namespace  com.richhickey.foil
{
	/// <summary>
	/// Summary description for IRuntimeServer.
	/// </summary>
	public interface IRuntimeServer
	{
		void processMessages(TextReader ins,TextWriter outs);
	}
}
