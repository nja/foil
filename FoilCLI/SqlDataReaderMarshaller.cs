/*
 * Created on Dec 10, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
using System;
using System.IO;
using System.Reflection;
using System.Data.SqlClient;
/**
 * @author Eric Thorsen
 *
 */

namespace com.richhickey.foil	
{
	/// <summary>
	/// Writes a SqlDataReader to an alist. The Columns key is paired with a simple vector of column names
	/// that are in the ordinal matching the list of data vectors that follow.
	/// The Data key yields a list of simple vectors that contains the data.
	/// (	(COLUMNS . #(NAME DATETIME VALUE VALUETYPE DESCRIPTION CONFIGCATEGORYID)) 
	///		(DATA . #("First row of data" NIL "C:EASTempLogs" "System.String" "Audit trail prefix" 3)
	///				#("2nd row of data" NIL "C:EASTempLogs" "System.String" "Audit trail prefix" 3))))
	/// </summary>
	public class SqlDataReaderMarshaller:IMarshaller
	{
		public void marshall(Object o, TextWriter w, IBaseMarshaller baseMarshaller,int flags, int depth)  
		{
			if(o.GetType()!=typeof(SqlDataReader))
				throw new ArgumentOutOfRangeException(
							String.Format("Expected DataReader in DataReaderMarshaller but got {0}",o.GetType().ToString()));
			SqlDataReader	dr	=	(SqlDataReader)o;
			// First write out the fields as an alist
			w.Write("((columns . #(");
			for(int i=0;i<dr.FieldCount;++i)
				w.Write("{0} ",dr.GetName(i));
			w.Write(")) ");
			// Write out the data as a list of vectors
			w.Write("(data . (");
			while(dr.Read())
			{
				w.Write("#(");
				for(int i=0;i<dr.FieldCount;++i)
					baseMarshaller.marshallAtom(dr.GetValue(i),w,flags,depth);
				w.Write(" ) ");
			}
			w.Write(")))");
		}
	}
}
