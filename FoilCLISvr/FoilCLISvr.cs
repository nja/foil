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
using System.IO;
using System.Collections;
using com.richhickey.foil;

namespace com.richhickey.foil
{
	/// <summary>
	/// Summary description for Class1.
	/// </summary>
	class FoilCLISvr
	{
		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args)
		{
			for(;;)
			{
			IReferenceManager referenceManager = new ReferenceManager();
			BaseMarshaller baseMarshaller = new BaseMarshaller(referenceManager);
			baseMarshaller.registerMarshaller(Type.GetType("System.Object"), new UniversalMarshaller());
			IReader reader = new MessageReader(referenceManager);
			IReflector reflector = new Reflector();
			IRuntimeServer server = new RuntimeServer(reader,baseMarshaller,referenceManager,reflector);
			try{
		        server.processMessages(Console.In,Console.Out);
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
