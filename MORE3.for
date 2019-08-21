**************************************************************************
*                         TherModynamic (FE.O.MN.SI)                         *
*                                                                        *
* A PROGRAM FOR Calculation OF Metal and slag reactions                  *                   
*                                                                        *
*                                                                        *
* DEVELOPED BY :  Afshin Heidari Monfared                                *
*                                                                        *
* FOR          :  Who want to use                                        *
*                                                                        *
* DATE         :  OCTOBER 23, 2013                                       *
*                                                                        *
**************************************************************************
      program tqex01
      implicit double precision (a-h, o-z)
C...dimension of the workspace should be large enough
      parameter(nwg=690000)
C
      dimension iwsg(nwg),indexc(100)
	DIMENSION VALNP(200),VALMP(200),F(100,5000,5)
	dimension COMP(100),AC(100),CWF(100),CMF(100)
	dimension ICONN(50)
      character*24 names(15),name
      character*8 stavar, sname
      logical pstat, tqgsp, sg2err
      LOGICAL SG3ERR
      character*8 CHOS,CHOSF 
      Character*6 SUBR
      Character*72 MESS

      WRITE (*,*) '****************************************************'
      WRITE (*,*) '****************************************************'
      WRITE(*,61)
61    FORMAT(
     &' *                                                  *'/
     &' * Program For Calculation OF Metal&Slag reactions  *'/
C     &' '/
     &' *TherModynamic System:Fe.O.Mn.Si.Ca.Al,Mg,C,Na,Ti,P*'/
     &' *                                                  *'/
     &' *     DEVELOPED BY :  Afshin Heidari Monfared      *'/
     &' *                                                  *'/
     &' *     For :  Who want to use                       * '/
C     &' '/
     &' *  Date :  October 23, 2013                        *'/)
	WRITE (*,*) '****************************************************'
      WRITE (*,*) '****************************************************'

           print *
	     print *
	     print *
	     print *
	     print *

      KT=0
C...initiate the workspace
      call tqini(nwg,iwsg)

C...read the thermodynamic data file which was created by using
C...the GES module inside the Thermo-Calc software package
      call tqrfil('SLAG1',iwsg)

C...get component names in the system
      call tqgcom(icom,names,iwsg)
      print *, 'This system has the following components:'
      print *, (names(i),i=1,icom)
      print *
      print *
      call tqgnp(iph, iwsg)
	IDEFG=icom+2
      print *, 'This system has', iph, ' phases:'
      do 1162 i=1, iph
	call tqgpn(i, name, iwsg)
         pstat=tqgsp(i, sname, an, iwsg)
         print *, i ,'   ', name, '  ', sname, ' ', an
 1162   continue
C...set the units of some properties.
      call tqssu('ENERGY','J',iwsg)

C      TEMP=1590.D0
401   print *
      print *

      do 125 i = 1, icom

      WRITE (*,*) '    Element is =>','                ', names(i)
      WRITE (*,*) 'Old pecrent value is=','   ', CWF(I)
      WRITE (*,*) 'Old mass value (Kg) is=','   ', CMF(I)
      print *
920   WRITE (*,*) 'Do you like to define New percent or Weight fraction
     + ,Yes(Y),No(N)?'

      print *
      WRITE (*,*) '='
      READ(*,*) CHOSF
      IF ((CHOSF.NE.'Y').AND.(CHOSF.NE.'N')) GOTO	920
      IF (CHOSF.EQ.'N') GOTO 125
      IF (CHOSF.EQ.'Y') GOTO 101


101   WRITE (*,*) 'Mass(M) Or Weight Percent(W)= ?'
	print *
      WRITE (*,*) '='
220	READ(*,*)  CHOS
      IF ((CHOS.NE.'W').AND.(CHOS.NE.'M')) GOTO	101 

      IF (CHOS.EQ.'W') THEN
C	WRITE (*,*) names(i), ' = ','In Percent'
	print *
	WRITE (*,*) '='
      READ(*,*) VALS
      CWF(I)=VALS
      
	CALL TQGSCI(IC3,names(i),IWSG)
	KT=KT+1
	CALL TQSETC('W%',-1,IC3,CWF(I),ICONN(KT),IWSG)
	IDEGF1=IDEFG-ICONN(KT)
	WRITE (*,*) 'Degree of freedom=',IDEGF1
	ENDIF
      IF (CHOS.EQ.'M') THEN
C	WRITE (*,*) names(i), ' = ','Mass (Kg)'
	print *
	WRITE (*,*) '='
      READ(*,*) VALS
      CMF(I)=VALS
      CALL TQGSCI(IC1,names(i),IWSG)
	KT=KT+1
      call tqsetc('M', -1, IC1, CMF(I), ICONN(KT), iwsg)
	IDEGF1=IDEFG-ICONN(KT)
	WRITE (*,*) 'Degree of freedom=',IDEGF1
	ENDIF
125	CONTINUE

           print *
	     print *
      WRITE (*,*) 'Old temperature value is=','   ', TEMP
      print *
924   WRITE (*,*) 'Do you like to define New Temperature value
     + ,Yes(Y),No(N)?'

      print *
	WRITE (*,*) '='
      READ(*,*)  CHOSF
      IF ((CHOSF.NE.'Y').AND.(CHOSF.NE.'N')) GOTO	924
      IF (CHOSF.EQ.'N') GOTO 24
      IF (CHOSF.EQ.'Y') GOTO 161


c cccccccccccccccccccc
      print *
	
161   WRITE (*,*)'Temperature in Kelvin Or celcious= ?'
      print *
      WRITE (*,*)'K=Kelvin,C=Celcious'
	print *
	WRITE (*,*) '='
      READ(*,*)  CHOS
      IF ((CHOS.NE.'C').AND.(CHOS.NE.'K')) GOTO	161 

      call tqssu('T',CHOS,iwsg)

30    WRITE(*,*) ' Temperature = ?'
      print *
	WRITE (*,*) '='
      READ(*,*) TEMP
	Pressure=101325.D0
      KT=KT+1
      call tqsetc('P',-1,-1,Pressure,ICONN(KT),iwsg)
	IDEGF1=IDEFG-ICONN(KT)
	WRITE (*,*) 'Degree of freedom=',IDEGF1
	KT=KT+1
      call tqsetc('T',-1,-1,TEMP,ICONN(KT),iwsg)
	IDEGF1=IDEFG-ICONN(KT)
      WRITE (*,*) 'Degree of freedom=',IDEGF1


C      CALL TQGPI(INDEXPF,'CAO',IWSG)
C      CALL TQSETC('NP',INDEXPF,-1,100,ICONN(KT),IWSG)
C	IDEGF1=IDEFG-ICONN(KT)
C      WRITE (*,*) 'Degree of freedom=',IDEGF1
C      CALL TQGPI(INDEXPF,'CAO',IWSG)
C      CALL TQGPCI(INDEXPF,INDEXCF,'CAO',IWSG)
C      CALL TQSETC('IN',INDEXPF,INDEXCF,100,ICONN(KT),IWSG)
C	IDEGF1=IDEFG-ICONN(KT)
C C     WRITE (*,*) 'Degree of freedom=',IDEGF1
C	XMN=2D-2
C	AL=2D-4
C	SI=2D-2
C	OO=1D-2
C	CC=1D-2
c.....get index of companent
      do 890 i=1,icom
	call TQGSCI(JKL,names(i),IWSG)
	INDEXC(I)=JKL
890   continue





24    WRITE (*,*)'Solve method  by Ordinary'
      call tqce(' ', 0, 0, 0.0D0, iwsg)



55     IF(SG3ERR(IERR,SUBR,MESS)) THEN
      WRITE (*,*) MESS
	ENDIF
     
C     Get total mass of system


798      CALL TQGET1('M',-1,-1,TOTALMASS,IWSG)
      WRITE (*,*) 'Conditions:'
	WRITE (*,*) 'Temperature =',TEMP,'    ','Pressure Pa=',Pressure
   
	
	WRITE (*,*) 'The system has following component'
      do 123 i = 1, icom
      IF (CMF(I).EQ.0) THEN
	CMF(I)=TOTALMASS*CWF(I)/100
      ENDIF
      IF (CWF(I).EQ.0) THEN
	CWF(I)=(CMF(I)/TOTALMASS)*100
      ENDIF 
	WRITE (*,*) 'Percent of element is=',names(i), ' = ',CWF(I)
	WRITE (*,*) '  Mass(Kg)of element is=',names(i), ' = ',CMF(I)
	WRITE (*,*) '  Degree of freedom = ',ICONN(I)
123	CONTINUE
110   format(2x,A,2(3x,f6.5))
      print *
	WRITE (*,120) '        Total Mass Kg=',TOTALMASS
120   format(2x,A,2(3x,f13.5))
      call tqget1('G',-1,-1,val,iwsg)
	print *
      print *, 'Gibbs energy of the whole system = ', val, ' J.'
c...find the equilibrium phase(s) and their compositions
      call tqget1('n', -1, -1, valnN, iwsg)
	print *
      Write (*,*) 'Number of all system components =', valnN
      call tqget1('H', -1, -1, valnH, iwsg)
	print *
      print *, 'Enthalpy of the whole system =', valnH, ' J.'
 10   format(2x,A,2(10x,f13.3))
      do 100 i = 1, iph
         call tqget1('DG', i, -1, val, iwsg)
         if(val.eq.0.0D0) then
            call tqgpn(i, name, iwsg)
      call tqget1('np', i, -1, valnp(I), iwsg)
	call tqget1('MP', i, -1, valMP(I), iwsg)
      DO 67 J = 1, icom
            call tqget1('W', i, indexc(J), valw, iwsg)
      call tqget1('AC', i, indexc(J), valAC, iwsg)
      Comp(j)=vaLW
      AC(j)=valAC
67    CONTINUE
c      Write (*,*) name
      Write (*,*) name
	Write (*,*) 'Number Of Moles =', valnp(I), 'Mass Kg = ',valMP(I)

	print *, 'Elements,                  Composition,   
     +          Activity'   
      DO 68 J = 1, icom
      Write (*,*) names(j),' ',Comp(j),' ',AC(j)
68    continue
         endif
 100  continue

Cccccccccccccccccccccccccccccccccccc
cCcccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccc
Cccccccccccccccccccccccccccccccccccc


166   WRITE (*,*)'Continue Or End?'
      WRITE (*,*)'C=Continue','E=End'
	WRITE (*,*) '='
      READ(*,*)  CHOS
	IF ((CHOS.NE.'C').AND.(CHOS.NE.'E')) GOTO	166 
      IF (CHOS.EQ.'E') GOTO 402 
      
	
	IF (CHOS.EQ.'C') THEN
854	WRITE (*,*)'Same condition:S or Change degree of freedom:D ?'
      READ (*,*) CHOSF
      IF ((CHOSF.NE.'S').AND.(CHOSF.NE.'D')) GOTO	854
	ENDIF
      IF (CHOSF.EQ.'S') GOTO 301
      IF (CHOSF.EQ.'D') THEN
      WRITE (*,*)'Which degree of freedom should be remove?'
	READ(*,*) NUMCON
	CALL TQREMC(NUMCON,IWSG)
	CMF(NUMCON)=0
      CWF(NUMCON)=0
      ENDIF

C765   CONTINUE


301   WRITE (*,*)'Only Change One Temperature Yes(Y) Or No(N)= ?'
      WRITE (*,*) '='	
      READ(*,*)  CHOS
      IF (CHOS.EQ.'Y') GOTO 30 
      IF (CHOS.EQ.'N') GOTO 405
405   WRITE (*,*)'Only Change only one Compound Yes(Y) Or No(N)= ?'
      WRITE (*,*) '='	
      READ(*,*)  CHOS
      IF (CHOS.EQ.'Y') GOTO 401 
      IF (CHOS.EQ.'N') GOTO 779
779   WRITE (*,*)'Temperature Or Component,T=Temperature,C=Componenent?'	
      WRITE (*,*) '='
	READ(*,*)  CHOS
      if (CHOS.eq.'C') GOTO 776
      if (CHOS.eq.'T') GOTO 796
776   WRITE (*,*)'Which Component= ?'
      WRITE (*,*) '='	
      READ(*,*)  CHOS
      IASK=0
      do 195 i = 1, icom
	if (names(i).eq.chos) then
	WRITE (*,*)'Enter low level= ?'
	WRITE (*,*) '='
	READ(*,*)  XLEVEL
	WRITE (*,*)'Enter high level= ?'
	WRITE (*,*) '='
	READ(*,*)  HLEVEL
1010  WRITE (*,*) 'Mass(M) Or Weight Percent(W)= ?'
	print *
	WRITE (*,*) '='
2200	READ(*,*)  CHOS
      IF ((CHOS.NE.'W').AND.(CHOS.NE.'M')) GOTO	1010
      DO 6901 YC= XLEVEL,HLEVEL,0.1
      IF (CHOS.EQ.'W') THEN
	CWF(I)=YC
	WRITE (*,*) names(i), ' = ','In Percent'
      CALL TQGSCI(IC3,names(i),IWSG)
C	KT=KT+1
      CALL TQSETC('W%',-1,IC3,CWF(I),ICONN(KT),IWSG)
	IDEGF1=IDEFG-ICONN(KT)
	WRITE (*,*) 'Degree of freedom=',IDEGF1
	ENDIF
      IF (CHOS.EQ.'M') THEN
	CMF(I)=YC
C	WRITE (*,*) names(i), ' = ','Mass (Kg)'
	print *
      CALL TQGSCI(IC1,names(i),IWSG)
C	KT=KT+1
      call tqsetc('M', -1, IC1,CMF(I), ICONN(KT), iwsg)
	IDEGF1=IDEFG-ICONN(KT)
	WRITE (*,*) 'Degree of freedom=',IDEGF1
	ENDIF

      WRITE (*,*)'Solve method  by Ordinary'
      call tqce(' ', 0, 0, 0.0D0, iwsg)

      CALL TQGET1('M',-1,-1,TOTALMASS,IWSG)
      WRITE (*,*) 'Conditions:'
	WRITE (*,*) 'Temperature =',TEMP,'    ','Pressure Pa=',Pressure
	WRITE (*,*) 'The system has following component'
      do 1237 II = 1, icom
      WRITE (*,*) 'Percent of element is=',names(iI), ' = ',CWF(II)
	WRITE (*,*) '  Mass(Kg)of element is=',names(iI), ' = ',CMF(II)
1237	CONTINUE
      print *
      WRITE (*,120) '        Total Mass Kg=',TOTALMASS
	print *
      call tqget1('G',-1,-1,val,iwsg)
      print *, 'Gibbs energy of the whole system = ', val, ' J.'
c...find the equilibrium phase(s) and their compositions
      call tqget1('n', -1, -1, valnN, iwsg)
      print *
      Write (*,*) 'Number of all system components =', valnN
      call tqget1('H', -1, -1, valnH, iwsg)
      print *, 'Enthalpy of the whole system =', valnH, ' J.'
      do 1007 IL = 1, iph
         call tqget1('DG', IL, -1, val, iwsg)
         if(val.eq.0.0D0) then
            call tqgpn(IL, name, iwsg)
            call tqget1('np', IL, -1, VALNP(IL), iwsg)
	call tqget1('MP', IL, -1, VALMP(IL), iwsg)
      DO 677 J = 1, icom
            call tqget1('W', IL, indexc(J), valw, iwsg)
c            print 10, name(1:lens(name)), valw, VALNP(IL) 
      call tqget1('AC', IL, indexc(J), valAC, iwsg)
      Comp(j)=vaLW
      AC(j)=valAC
677    CONTINUE
      Write (*,*) name
	Write (*,*) 'Number Of Moles =', VALNP(IL), 'Mass Kg = ',VALMP(IL)
	print *, 'Elements,                  Composition,   
     +          Activity'

       
      DO 687 J = 1, icom
      Write (*,*) names(j),' ',Comp(j),' ',AC(j)
687    continue
         endif
 1007  continue

6901  CONTINUE
	ENDIF
195   CONTINUE
796   WRITE (*,*)'Enter low level= ?'
      WRITE (*,*) '='
	READ(*,*)  XLEVEL
	WRITE (*,*)'Enter high level= ?'
	WRITE (*,*) '='
	READ(*,*)  HLEVEL
	ITI=0
	OPEN (UNIT = 125, FILE = 'F.DAT')
	REWIND 125
	WRITE(125,*) 'TITLE = "Temperature and mass"'
      WRITE(125,*) 'VARIABLES = "X", "Y", "Z"'
      WRITE(125,*) 'ZONE I=40, J=3, K=2, F=POINT'
	CLOSE (UNIT=125)
      DO 6902 YC= XLEVEL,HLEVEL,1
C	KT=KT+1
      call tqsetc('T',-1,-1,YC,ICONN(KT),iwsg)


241    WRITE (*,*)'Solve method  by Ordinary'
      call tqce(' ', 0, 0, 0.0D0, iwsg)
551   IF(SG3ERR(IERR,SUBR,MESS)) THEN
      WRITE (*,*) MESS
	ENDIF

      CALL TQGET1('M',-1,-1,TOTALMASS,IWSG)
      WRITE (*,*) 'Conditions:'
	WRITE (*,*) 'Temperature =',YC,'    ','Pressure Pa=',Pressure
	WRITE (*,*) 'The system has following component'
      do 1437 II = 1, icom
      WRITE (*,*) 'Percent of element is=',names(iI), ' = ',CWF(II)
	WRITE (*,*) '  Mass(Kg)of element is=',names(iI), ' = ',CMF(II)
1437	CONTINUE
      print *
      WRITE (*,120) '        Total Mass Kg=',TOTALMASS
	print *
      call tqget1('G',-1,-1,val,iwsg)
      print *, 'Gibbs energy of the whole system = ', val, ' J.'
c...find the equilibrium phase(s) and their compositions
      call tqget1('n', -1, -1, valnN, iwsg)
	print *
      Write (*,*) 'Number of all system components =', valnN
      call tqget1('H', -1, -1, valnH, iwsg)
      print *, 'Enthalpy of the whole system =', valnH, ' J.'
      do 1047 IL = 1, iph
         call tqget1('DG', IL, -1, val, iwsg)
         if(val.eq.0.0D0) then
            call tqgpn(IL, name, iwsg)
      call tqget1('np', IL, -1, VALNP(IL), iwsg)
	call tqget1('MP', IL, -1, VALMP(IL), iwsg)
      DO 647 J = 1, icom
            call tqget1('W', IL, indexc(J), valw, iwsg)
      call tqget1('AC', IL, indexc(J), valAC, iwsg)
      Comp(j)=vaLW
      AC(j)=valAC
647    CONTINUE
      ITI=ITI+1
      F(IL,ITI,1)=YC
	F(IL,ITI,2)=VALMP(IL)
	F(IL,ITI,3)=VALNP(IL)     


      OPEN (UNIT = 125, FILE = 'F.DAT')
C	REWIND 125
	
      WRITE(125,*)  YC,VALMP(3),VALMP(4)

      Write (*,*) name
	Write (*,*) 'Number Of Moles =', VALNP(IL), 'Mass Kg = ',VALMP(IL)
	print *, 'Elements,                  Composition,   
     +          Activity'   
      DO 6247 J = 1, icom
      Write (*,*) names(j),' ',Comp(j),' ',AC(j)
6247   continue
      endif
1047  continue
6902  CONTINUE

      print *, 'This system has', iph, ' phases:'
      do 1163 i=1, iph
	call tqgpn(i, name, iwsg)
         pstat=tqgsp(i, sname, an, iwsg)
         print *, i ,'   ', name, '  ', sname, ' ', an
 1163 CONTINUE

7871  WRITE(*,*)  'WHICH PHASE DO YOU LIKE TO SEE, Enter Index'
      print *
      WRITE (*,*) '='
      READ(*,*)  INDS

C     *      REWIND 225
      OPEN (UNIT = 225, FILE = 'FX.DAT')
	REWIND 225
	WRITE(225,*) 'TITLE = "Temperature and mass"'
      WRITE(225,*) 'VARIABLES = "X", "Y"'
      WRITE(225,*) 'ZONE I=40, J=2, K=1, F=POINT'
      DO 828 IKI=1,ITI 
	IF (F(INDS,IKI,1).NE.0) THEN
      WRITE(225,*) F(INDS,IKI,1),F(INDS,IKI,2)
      ENDIF
828   CONTINUE
      CLOSE (UNIT=225)


      OPEN (UNIT = 226, FILE = 'FY.DAT')
	REWIND 226
	WRITE(226,*) 'TITLE = "Temperature and Activity"'
      WRITE(226,*) 'VARIABLES = "X", "Y"'
      WRITE(226,*) 'ZONE I=40, J=2, K=1, F=POINT'
	DO 823 IKI=1,ITI 
	IF (F(INDS,IKI,1).NE.0) THEN
      WRITE(226,*) F(INDS,IKI,1),F(INDS,IKI,3) 
      ENDIF
823	CONTINUE
      CLOSE (UNIT=226)

888   WRITE (*,*) 'Do you like to see more graph,Yes(Y),No(N)?'
      print *
      WRITE (*,*) '='
      READ(*,*) CHOSF
      IF ((CHOSF.NE.'Y').AND.(CHOSF.NE.'N')) GOTO	888
      IF (CHOSF.EQ.'N') GOTO 777
      IF (CHOSF.EQ.'Y') GOTO 7871


777   WRITE (*,*) 'Do you like to Contiune,Yes(Y),No(N)?'
      print *
      goto 166


	print *

402   stop
      end


C      print *, 'W(Mn)=',XMN

C      do 12 i=1, iph
C         call tqgpn(i, name, iwsg)
C	CALL TQCSP (I,'suspend',0,IWSG)
C         pstat=tqgsp(i, sname, an, iwsg)
C        print *, i ,'   ', name, '  ', sname, ' ', an
C 12   continue


C      CALL TQGPI(IJ,'FE-LIQUID',IWSG)
C      CALL TQCSP(IJ,'ENTERED',0,IWSG)
C      CALL TQGPI(IV,'SLAG',IWSG)
C      CALL TQCSP(IV,'ENTERED',0,IWSG)

C      CALL TQGPI(IB,'GAS',IWSG)
C      CALL TQCSP(IB,'ENTERED',0,IWSG)
C      do 112 i=1, iph
C	call tqgpn(i, name, iwsg)
C         pstat=tqgsp(i, sname, an, iwsg)
C         print *, i ,'   ', name, '  ', sname, ' ', an
C 112   continue


c...find the equilibrium liquidus and solidus temperature of this alloy
c...
c...change the status of the liquid phase to FIXED with amount 1
c...and then remove the temperature condition
C      call tqgpi(iliq,'liq',iwsg)
C      call tqcsp(iliq, 'FIXED', 1.0D0, iwsg)
C      call tqremc(icont,iwsg)
C      call tqce(' ',0,0,0.0D0,iwsg)
C	CALL TQLE(IWSG)
C	CALL TQLC(IWSG)

C      print *, 'At T = 500.0 C, Wt% Mg = 4'

C      CALL TQGPI(IND,'FE-LIQUID',IWSG)
C      CALL TQGPCI(IND,INC,'FE',IWSG)
C      CALL TQSETC('IN',IND,INDC,1.5D5,NCOND,IWSG)


C...get number of phases in the system
C      call tqgnp(iph, iwsg)
C C     print *, 'This system has', iph, ' phases:'

C...get names and status of the phases in the system
C      do 10 i=1, iph
C         call tqgpn(i, name, iwsg)
C         pstat=tqgsp(i, sname, an, iwsg)
C         print *, i ,'   ', name, '  ', sname, ' ', an
C 10   continue





C      CALL TQGSCI(IC2,'C',IWSG)
C	CALL TQSETC('W',-1,IC2,4D-4,NCONA,IWSG)



C	WRITE (*,110) 'W(Mn)=',XMN
C	WRITE (*,110) 'W(Al)=',AL
C	WRITE (*,110) 'W(O)=',OO
C	WRITE (*,110) 'W(Si)=',SI


C...set the condition for a sigle equilibrium calculation

C     call tqsetc('N', -1, -1, 1.00D6, ICONN(KT), iwsg)

C      call tqsetc('W%',-1, 2, 4.0D0, iconw, iwsg)
C      CALL TQGSCI(IC1,'FE',IWSG)
C C     call tqsetc('M', -1, IC1, 1.00D3, ICONN(KT), iwsg)
C      CALL TQGSCI(IC3,'MN',IWSG)
C	CALL TQSETC('W',-1,IC3,XMN,NCONB,IWSG)     
C      CALL TQGSCI(IC4,'AL',IWSG)
C	CALL TQSETC('W',-1,IC4,AL,NCONC,IWSG)
C      CALL TQGSCI(IC5,'Si',IWSG)
C	CALL TQSETC('W',-1,IC5,SI,NCONn,IWSG)
C      CALL TQGSCI(IC6,'O',IWSG)
C	CALL TQSETC('W',-1,IC6,OO,NCONV,IWSG)
C      CALL TQGSCI(IC6,'C',IWSG)
C	CALL TQSETC('W',-1,IC6,CC,NCONV,IWSG)
C...calculate equilibrium
C      CALL TQSNL(500,1E-6,1E-14,'Y',IWSG)
C      CALL TQSNL(500,1E-6,1E-14,'Y',IWSG)
C       call tqce(' ', 0, 0, 0.0D0, iwsg)
C	CALL TQSNL(500,1E-6,1E-14,'Y',IWSG)
C	CALL TQCEG(IWSG)

C99    WRITE (*,*)'Solve method Global Minimization(G)-Ordinary(O)'
C      READ(*,*)  CHOS
C      IF (CHOS.EQ.'G') GOTO 23 
C      IF (CHOS.EQ.'O') GOTO 24
C      IF ((CHOS.NE.'G').AND.(CHOS.NE.'O')) GOTO	99 
C      GOTO 55
C23    WRITE (*,*)'Solve method  by Global'
C      CALL TQCEG(IWSG)

      	
C	DO 765 IT=1,1000
C      XX=0.004*IT
C	XY=0.0016*IT
C      CALL TQGSCI(IC1,'CA',IWSG)
C      call tqsetc('M', -1, IC1, XX, ICONN(KT), iwsg)
C	CALL TQGSCI(ICM,'O',IWSG)
C      call tqsetc('M', -1, ICM, XY, ICONN(KT), iwsg)

C      call tqce(' ', 0, 0, 0.0D0, iwsg)



      

