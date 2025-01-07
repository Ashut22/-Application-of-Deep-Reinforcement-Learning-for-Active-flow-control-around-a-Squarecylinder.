cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine tigrad
c
c       maximum of the velocity-alternation during a zeit increment
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(*,*)'entered tigrad'
c
	dtmax=0.
	dumax=0.
	dvmax=0.
c
	do 180 i=2,(ire-1)
	do 180 j=2,jre
c
	dudt=dabs(u1(i,j)-u2(i,j))/deltat
	dvdt=dabs(v1(i,j)-v2(i,j))/deltat
c
	dact=dtmax
	dtmax=dmax1(dtmax,dudt,dvdt)
	if(dtmax.le.dact) goto 180
	idtm=i
	jdtm=j
 180	continue
c
c ****** constant properties ********
        open(27,file='prolt',form='formatted')
	write(27,101)ita,iti,isum,dtmax,deltat,alpha
 101    format(2x,3i7,2x,2e13.5,e13.5)
	close(27)
c
	write(6,182)idtm,jdtm,dtmax
	write(68,999)ita,iti,isum,dtmax,idtm,jdtm,deltat,p(2,50)
 999    format(2x,3i5,e13.5,2x,2i5,2x,e13.5,e13.5)
 182	format(2x,'maximum change in velocity ',2x,2i5,e13.5)
	if (dtmax*.1 .le. epsi) epsi = dtmax*.1
	return
	end
