/*
 * Created on Dec 8, 2004
 *
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 */
package com.richhickey.foil;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;
import java.util.*;

/**
 * @author Rich
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class MessageReader implements IReader
    {

    /* (non-Javadoc)
     * @see com.richhickey.foil.IReader#readMessage(java.io.Reader)
     */
    public List readMessage(Reader strm) throws IOException
        {
        return readSexpr(strm);
        }

	public List readSexpr(Reader strm)
	throws IOException
		{
	    
        int c = strm.read();
        while (Character.isWhitespace((char) c))
            c = strm.read();
        
        if (c != '(')
            {
            throw new IOException("expected sexpr to begin with '('");
            }
        List ret = new ArrayList();

        for(strm.mark(1);(c = strm.read()) != ')';strm.mark(1))
            {
            if (Character.isWhitespace((char) c))
                continue;
            switch (c)
                {
                case -1:
                    throw new IOException("unexpected EOF");
                case '(':
                    strm.reset();
                    ret.add(readSexpr(strm));
                    break;
                case '"':
                    strm.reset();
                    ret.add(readString(strm));
                    break;
                default:
                    strm.reset();
                    ret.add(readToken(strm));
                    break;
                }
            }
        return ret;
        }

    static protected String readString(Reader strm)
            throws IOException
        {
        int c = strm.read();
        if (c != '"')
            {
            throw new IOException("expected string to begin with '\"'");
            }
        StringBuffer sb = new StringBuffer();
        boolean end = false;
        while (!end)
            {
            c = strm.read();
            if (c == -1)
                throw new IOException("unexpected EOF");
            if (c == '"')
                end = true;
            else if (c == '\\')
                {
                c = strm.read();
                if (c == -1)
                    throw new IOException("unexpected EOF");
                if (c == '"' || c == '\\')
                    sb.append((char) c);
                else
                    throw new IOException("unsupported escape character: '"
                            + Character.toString((char) c) + "'");
                }
            else
                sb.append((char) c);
            }
        return sb.toString();
        }

    protected String readToken(Reader strm) throws IOException
        {
        StringBuffer sb = new StringBuffer();
        boolean end = false;
        for (strm.mark(1);!end;strm.mark(1))
            {
            int c = strm.read();
            if (c == -1)
                throw new IOException("unexpected EOF");
            if (c == ')' || c == '(' || Character.isWhitespace((char) c)
                    || c == '}')
                {
                strm.reset();
                end = true;
                }
            else
                sb.append((char) c);
            }
        String ret = sb.toString();
        return ret;
        }
    
    public static void main(String[] args)
        {
        BufferedReader strm = new BufferedReader(new InputStreamReader(System.in));
        IReader rdr = new MessageReader();
        for(;;)
            {
            try{
                List msg = rdr.readMessage(strm);
                System.out.println(msg.toString());
            	}
            catch(Exception ex)
            	{
                System.out.println(ex.getMessage());
                break;
            	}
            }
        }
    }
