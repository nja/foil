using System;
using System.IO;

namespace FoilForDotNet
{
	/// <summary>
	/// Summary description for IMarshaller.
	/// </summary>
	public interface IMarshaller
	{
		public	void marshall(Object o,TextWriter w, IMarshaller baseMarshaller, int flags,int depth);
	}
}
