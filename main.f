cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
c
c
         program main 
c
c******************************************************************
c*       program main for computing flow field in a channel 
c*       with built in square cylinder
c******************************************************************
c
	include 'header'
c	
c 	write(*,23)
c23      format('has entered main')
c
	open(68,file='pro',form='formatted')
c	open(2,file='frea1000',form='formatted')
c	open(12,file='cl1000',form='formatted')
c	open(17,file='cd1000',form='formatted')
c	open(87,file='phase1',form='formatted')
c	open(37,file='phase2',form='formatted')
c	open(47,file='phase3',form='formatted')
c	open(57,file='phase4',form='formatted')
c	open(67,file='phase5',form='formatted')
c
	call init
c
	call non_uni_coeff
c
	if (irest.eq.0) call start
	if (irest.eq.1) call restar
 10     call conti
	open(3,file='iwrite',form='formatted')
	read(3,*)iwrite
	read(3,*)itime
	read(3,*)istop
	close(3)	
c
c
	if (istop.eq.1) then
	call otre
	end if
	call bcns
	call velalt
	call ticorr
	call otpt
c	call energy
	call bcyl2d
c
	call nseqcp
	call bcns
	call tigrad
c
c	*******************************
c
c	if(ita.gt.1400) epsi=0.00001
c 	if(ita.lt.1400 .and. ita.gt.1200)epsi=0.000025
c 	if(ita.lt.1200 .and. ita.gt.1000)epsi=0.00005
c 	if(ita.lt.1000 .and. ita.gt.900)epsi=0.0001
c 	if(ita.lt.900 .and. ita.gt.800)epsi=0.00025
c 	if(ita.lt.800 .and. ita.gt.700)epsi=0.0005
c 	if(ita.lt.700 .and. ita.gt.600)epsi=0.001
c 	if(ita.lt.600 .and. ita.gt.500)epsi=0.005
c 	if(ita.lt.500 .and. ita.gt.400)epsi=0.01
c 	if(ita.lt.400 .and. ita.gt.300)epsi=0.025
c 	if(ita.lt.300 .and. ita.gt.200)epsi=0.05
c 	if(ita.lt.200 .and. ita.gt.100)epsi=0.075
c 	if(ita.lt.100 .and. ita.gt.50)epsi=0.1
c 	if(ita.lt.50)epsi=0.5
c
	if(irest.eq.1)irest=0
c
c 	write(*,123) epsi
c 123    format(  'epsi = ',e10.4,$)
	goto 10
c	*******************************
c
	end
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
