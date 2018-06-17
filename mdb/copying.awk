BEGIN	{
	  FS="\"";
	  print "/* Do not modify this file; it is created automatically"
	  print "   by copying.awk.  */"
	  print "char * copying_info[] = {"
	}
	{ printf "\t\"%s\",\n", $0
	}
END	{
	  print "};"
	  print "int copying_info_lines = sizeof(copying_info) / sizeof(char *);"
	}
