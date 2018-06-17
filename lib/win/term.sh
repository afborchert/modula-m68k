#------------------------------------------------------------------------------
# generate TermInfo.d from /usr/5include/term.h
# afb 5/88
# rev afb 12/90: adapted to SunOS 4.1
#------------------------------------------------------------------------------

	PATH=/bin:/usr/bin	# be sure to call commands we know about
	tmp=/tmp/ti$$
	tmp2=/tmp/ti_$$

trap "rm -f $tmp $tmp2" 0 1 2 3 15

# part 1,2,3: strings
#      4:     booleans
#      5:     integers
sed 's/,\/\*/ \/*/; s/[,;]//g' </usr/5include/term.h \
| sed 's/_crsr_/_cursor_/; s/_dlt_/_delete_/; s/_entr_/_enter_/
       s/_ins_/_insert_/; s/_ky_/_key_/; s/_kpad_/_keypad_/
       s/_prm_/_parm_/; s/_scrll_/_scroll_/; s/_mem_/_memory_/' \
| awk '
BEGIN			{ copy = 0; part = 0;
			  bools = 0; ints = 0; strings = 0;
			  upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			  lower="abcdefghijklmnopqrstuvwxyz"
			}
/struct strs {/		{ part = 1 }
/struct strs2 {/	{ part = 2 }
/struct strs3 {/	{ part = 3 }
/struct _bool_struct {/	{ part = 4 }
/struct _num_struct {/	{ part = 5 }
/charptr/		{ if (part) copy = 2 }
/^[ 	]*char[ 	]*/ { if (part) copy = 2 }
/^[ 	]*short[ 	]*/ { if (part) copy = 2 }
/struct _str_struct/	{ copy = 0; part = 0 }
/Sentinel/		{ copy = 0 }
/^}/			{ copy = 0 }

			{ if (copy == 2)
				copy = 1;
			  else if (copy == 1)
			  {	split($1, comp, "_");
				i = 2; out = comp[1];
				while (comp[i] != "")
				{	head = substr(comp[i], 1, 1);
					tail = substr(comp[i], 2);
					letter = index(lower, head);
					if (letter)
						head = substr(upper, letter, 1);
					out = out head tail;
					++ i;
				}
				if (part == 1 || part == 2 || part == 3)
				{	printf "%s: String;\n", out
					++ strings;
				}
				else if (part == 4)
				{	printf "%s: BOOLEAN;\n", out
					++ bools;
				}
				else if (part == 5)
				{	printf "%s: INTEGER;\n", out
					++ ints;
				}
			  }
			}
END			{	printf "strings = %d;bools = %d;ints = %d;\n", \
					strings, bools, ints >"'$tmp2'"
			}
' >$tmp

ex TermInfo.t >/dev/null <<eof
/Term =/
+
r $tmp
/CONST/
r $tmp2
1,\$!m2b
w! TermInfo.d
eof
