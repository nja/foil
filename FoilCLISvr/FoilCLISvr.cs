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
using System.Reflection;
using System.IO;
using System.Collections;
using System.Threading;
using com.richhickey.foil;

namespace com.richhickey.foil
{
	class FoilCLISvr
	{
		[STAThread]
		static void Main(string[] args)
		{
			for(;;)
			{
				IReferenceManager referenceManager	= new ReferenceManager();
				BaseMarshaller baseMarshaller		= new BaseMarshaller(referenceManager);
				baseMarshaller.registerMarshaller(Type.GetType("System.Object"), new UniversalMarshaller());
				IReflector	reflector				=	new Reflector(baseMarshaller);
				IReader reader						= new MessageReader(referenceManager,reflector);
				RuntimeServer server				= new RuntimeServer(reader,baseMarshaller,referenceManager,reflector);
				try 
				{
					if(args.Length >= 1) //port #s, run on sockets
					{	
						//fire up a background thread for all sockets except first
						for(int i=1;i<args.Length;i++)
						{
							RuntimeServer	rs		= server;
							Int32			port	= Int32.Parse(args[i]);
							RuntimeSocketServer	rts	= new RuntimeSocketServer(rs,port);
							//Do I need to reference these?
							(new Thread(new ThreadStart(rts.processMessagesOnSocket))).Start();
						}
						//app lives with first socket
						RuntimeSocketServer	mainRts	= new RuntimeSocketServer(server,Int32.Parse(args[0]));
						mainRts.processMessagesOnSocket();
					}
					else //run on stdio
					{
						server.processMessages(Console.In,Console.Out);
					}
				}
				catch(Exception ex)
				{
					Console.WriteLine(ex.Message);
					break;
				}
			}
		}
	}
}
