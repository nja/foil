using System;
using System.IO;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReader.
	/// </summary>
	public interface IReader
	{
			public	ArrayList readMessage(TextReader strm);
	}
}
