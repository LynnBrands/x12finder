-- Read ASN, provide summary of total UPC units

include get.e
atom ifp, ofp
sequence table

--------------------------------------------------------------------------------

function x12_parse_seg(sequence line, atom elemterm)

  sequence seg
  atom i1, i2

            seg = {}
            i1 = 1
            i2 = 0
            for i = 1 to length(line) do
              if line[i] = elemterm then
                i2 = i-1
                seg = append(seg,line[i1..i2])
                i1 = i + 1
              end if
            end for
            seg = append(seg,line[i1..length(line)])


  return seg

end function

--------------------------------------------------------------------------------

function x12_isa_headers(sequence fname)
	atom fp,elemterm,segterm
	object line
	sequence hdrs hdrs = repeat("",18)
	fp = open(fname,"rb")
	if fp < 3 then
		-- write_log("I could not parse the X12 file.")
		return ""
	end if
	line = get_bytes(fp,106)
	close(fp)
	if sequence(line) and length(line)=106 then
		elemterm = line[4]
		segterm = line[106]
		line = line[1..105]
	else
		return {}
	end if
	hdrs[1]=line[5..6]
	hdrs[2]=line[8..17]
	hdrs[3]=line[19..20]
	hdrs[4]=line[22..31]
	hdrs[5]=line[33..34]
	hdrs[6]=line[36..50] -- Sender ID
	hdrs[7]=line[52..53]
	hdrs[8]=line[55..69] -- Receiver ID
	hdrs[9]=line[71..76]
	hdrs[10]=line[78..81]
	hdrs[11]=line[83]
	hdrs[12]=line[85..89] -- Version
	hdrs[13]=line[91..99] -- ISA Control #
	hdrs[14]=line[101]
	hdrs[15]=line[103]
	hdrs[16]=line[105]
	hdrs[17]=elemterm
	hdrs[18]=segterm
	
	return hdrs
end function

--------------------------------------------------------------------------------

procedure main()

	object cmdline
	sequence isa, po, bol, dc, store, upc, qty, ctn, wgt, level
	object line, qval, rptstores
	atom char, found
	cmdline = command_line()
	table = {}
	if length(cmdline)<3 then
		puts(1,"Please specify the file to read.")
		return
	end if
	isa = x12_isa_headers(cmdline[3])
	ifp = open(cmdline[3],"rb")
	char = getc(ifp)
	line = ""
	while char >= 0 do
		if char!=isa[18] then
			line = line & char
		else
			line = x12_parse_seg(line,isa[17])
			found = 0
			if compare(line[1],"HL")=0 then
				level = line[4]
			elsif compare(line[1],"TD1")=0 and compare(level,"S")=0 then
				ctn = line[3]
				wgt = line[8]
			elsif compare(line[1],"REF")=0 and compare(line[2],"BM")=0 then
				bol=line[3]
			elsif compare(line[1],"N1")=0 and compare(line[2],"ST")=0 then
				dc = line[5]
			elsif compare(line[1],"PRF")=0 then
				po = line[2]
			elsif compare(line[1],"N1")=0 and (compare(line[2],"BY")=0 or compare(line[2],"Z7")=0) then
				store = line[5]
			elsif compare(line[1],"LIN")=0 and compare(line[3],"UP")=0 then
				upc = line[4]
			elsif compare(line[1],"SN1")=0 then
				qty = line[3]
				qval = value(qty)
				if qval[1]=GET_SUCCESS then
					qval = qval[2]
				else
					qval = 0
				end if
				found = 0
				for ctr = 1 to length(table) do
					if compare(table[ctr][1],po)=0 and compare(table[ctr][2],upc)=0 then
						found = 1
						table[ctr][3] += qval
						table[ctr][4] = append(table[ctr][4],{store,qval})
						exit
					end if
				end for
				if not found then
					table = append(table,{po,upc,qval,{{store,qval}}})
				end if
			end if
			line = ""
		end if
		char = getc(ifp)
	end while
	close(ifp)
	ofp = 1
	puts(ofp,"BOL "&bol&" summary, DC: "&dc&", Cartons: "&ctn&", Weight: "&wgt&"\n\n")
	for ctr = 1 to length(table) do
		puts(ofp,"PO: "&table[ctr][1]&"\tUPC: "&table[ctr][2]&sprintf("\tQuantity: %d\n",table[ctr][3]))
		if compare(table[ctr][1],"21021366")=0 and compare(table[ctr][2],"766965848179")=0 then
			rptstores = ctr
		end if
	end for
    for s = 1 to length(table[rptstores][4]) do
      puts(ofp,sprintf("Store: %s, Qty %d\n",table[rptstores][4][s]))
    end for
	if ofp > 2 then close(ofp) end if
end procedure

--------------------------------------------------------------------------------

main()
