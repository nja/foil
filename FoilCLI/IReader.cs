using System;
using System.IO;
using System.Collections;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for IReader.
	/// </summary>
	public interface IReader
	{
			ArrayList readMessage(TextReader strm);
	}
}
