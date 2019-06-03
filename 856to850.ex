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
	atom char, found, segcount
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
	segcount = 1
	puts(ofp,"ISA*00*          *00*          *08*6113310071     *01*099081168      *110825*0941*U*00405*999026090*0*P*>~")
	puts(ofp,"GS*PO*6113310071*099081168*20110825*0941*99926089*X*004050VICS~")
	puts(ofp,"ST*850*"&bol[12..16]&"~")
	puts(ofp,"BEG*07*RL*4016951MJS*1*20110726~")
	puts(ofp,"REF*BT*9913200017~")
	puts(ofp,"REF*DP*0121~")
	puts(ofp,"REF*PG*BRDG SPRTSWR~")
	puts(ofp,"FOB*DF~")
	puts(ofp,"ITD*02*8*8**10~")
	puts(ofp,"DTM*001*20110803~")
	puts(ofp,"DTM*064*20110801~")
	segcount = 9
	for ctr = 1 to length(table) do
		puts(ofp,"PO1*"&sprintf("%d*%d",{ctr,table[ctr][3]})&"*EA*9*WE*UP*"&table[ctr][2]&"*CG*30")
		for sdq = 1 to length(table[ctr][4]) do
			if remainder(sdq-1,10)=0 then
				segcount += 1
				puts(ofp,"~SDQ*EA*92")
			end if
			puts(ofp,"*"&table[ctr][4][sdq][1]&"*"&sprintf("%d",table[ctr][4][sdq][2]))
		end for
		segcount += 1
		puts(ofp,"~CTP**RTL*28~")
		segcount += 1
	end for
	puts(ofp,sprintf("CTT*%d~",length(table)))
	segcount += 1
	puts(ofp,"SE*"&sprintf("%d",segcount+2)&"*"&bol[12..16]&"~")
	puts(ofp,"GE*1*99926089~")
	puts(ofp,"IEA*1*999026090~")
	if ofp > 2 then close(ofp) end if
end procedure

--------------------------------------------------------------------------------

main()
