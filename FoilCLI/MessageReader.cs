/*
 * Created on Dec 7, 2004
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
using System.Collections;
using System.IO;
using System.Text;

namespace com.richhickey.foil
{
	public class MessageReader :IReader
	{
		public ArrayList readMessage(TextReader strm) 
		{
			return readSexpr(strm);
		}

		public ArrayList readSexpr(TextReader strm)
		{
			return readDelimitedList(strm,'(',')');
		}

		public ArrayList readDelimitedList(TextReader strm, int startc, int endc)
		{
	    
			int c = strm.Read();
			while (Char.IsWhiteSpace((char) c))
				c = strm.Read();
        
			if (c != startc)
				throw new IOException(String.Format("expected list to begin with; {0}",(char)startc));
			ArrayList ret = new ArrayList();
			for (c=strm.Peek();c!=-1;c=strm.Peek())
			{
				if (Char.IsWhiteSpace((char) c))
				{
					strm.Read();
					continue;
				}
				if(c==-1)
					throw new IOException("unexpected EOF");
				if(c==endc)
				{
					strm.Read();
					return	ret;
				}
				switch ((char)c)
				{
					case '(':
						ret.Add(readSexpr(strm));
						break;
					case '"':
						ret.Add(readString(strm));
						break;
					default:
						ret.Add(readToken(strm));
						break;
				}
			}
			return ret;
		}

		static protected String readString(TextReader strm)
		{
			int c = strm.Read();
			if (c != '"')
				throw new IOException("expected string to begin with '\"'");
			StringBuilder sb = new StringBuilder();
			Boolean end = false;
			while (!end)
			{
				c = strm.Read();
				if (c == -1)
					throw new IOException("unexpected EOF");
				if (c == '"')
					end = true;
				else if (c == '\\')
				{
					c = strm.Read();
					if (c == -1)
						throw new IOException("unexpected EOF");
					if (c == '"' || c == '\\')
						sb.Append((char) c);
					else
						throw new IOException(String.Format("unsupported escape character: '{0}'"
															,(char) c));
				}
				else
					sb.Append((char) c);
			}
			return sb.ToString();
		}

		protected String readToken(TextReader strm) 
		{
			StringBuilder sb	= new StringBuilder();
			int	c	=	strm.Peek();
			for (;c!=-1;c=strm.Peek())
			{
				if (c == -1)
					throw new IOException("unexpected EOF");
				if (
					c == ')' 
					|| c == '(' 
					|| Char.IsWhiteSpace((char) c)
					|| c == '}'
					)
					break;
				else
					sb.Append((char) strm.Read());
			}
			String ret = sb.ToString();
			return ret;
		}
	}
}