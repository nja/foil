using System;
using System.IO;

namespace  com.richhickey.foil
{
	/// <summary>
	/// Summary description for IMarshaller.
	/// </summary>
	public interface IMarshaller
	{
		void marshall(Object o,TextWriter w, IMarshaller baseMarshaller, int flags,int depth);
	}
}
