ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine ticorr
c
c       calculation of time-increment
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	utop=0.
	vtop=0.
c
	do 90 i=2,ire
	do 90 j=2,jre
	uchek=0.5*(u2(i,j)+u2(i-1,j))
	vchek=0.5*(v2(i,j)+v2(i,j-1))
	utop=dmax1(utop,abs(uchek))
	vtop=dmax1(vtop,abs(vchek))
 90	continue
c
	if(vtop.gt.0)then
c
	umax=deltax(i)/utop
	vmax=deltay(j)/vtop
c
	endif
c
	if(itime.eq.0)then
c
	if(vtop.le.0) goto 91
c
	deltat=dmin1(umax,vmax)
	goto 92
c
 91	continue
c
	deltat=dxmin/utop
c
 92 	continue
c
	deltch=0.5*(dxmin**2.0*dymin**2.0/
     $	       (dxmin**2.0+dymin**2.0))*re
c
c
	deltat=dmin1(deltat,deltch)
	deltat=stab*deltat
	endif
c	deltat=0.0005
c
	return
	end
