ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
         subroutine ceqcp
c
c      mass continuity equation for  constant properties
c	pressure correction being performed (?)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
      	divmax=0.
	dalt=0.
	imax=0
	jmax=0
c
	do 70 i=2,ire
	do 70 j=2,jre
c
	dxxr=0.5*(deltax(i)+deltax(i+1))
	dxxl=0.5*(deltax(i)+deltax(i-1))
	dyyt=0.5*(deltay(j)+deltay(j+1))
	dyyb=0.5*(deltay(j)+deltay(j-1))
c
	beta=beta0/(2.0*deltat*
     $	(1./(dxxr*deltax(i))+1./(dyyt*deltay(j))))
c
	div=(u2(i,j)-u2(i-1,j))/deltax(i)
     $    +(v2(i,j)-v2(i,j-1))/deltay(j)
        deltap=-beta*div                      
	a=deltat*deltap
c
	p(i,j)=p(i,j)+deltap
	u2(i,j)=u2(i,j)+a/dxxr
	u2(i-1,j)=u2(i-1,j)-a/dxxl
	v2(i,j)=v2(i,j)+a/dyyt
	v2(i,j-1)=v2(i,j-1)-a/dyyb
c
	dab=dabs(div)
	if(dab.gt.dalt) then
	divmax=dab
	imax=i
	jmax=j
	end if
	dalt=divmax
c
 70	continue
c	increment in number of iterations
	iti=iti+1
	isum=isum+1
c
	if(iti.eq.1 .or. iti.eq.(iti/50*50)) then
        write(*,71)ita,iti,isum,divmax,imax,jmax 
 71	format(2x,3i7,4x,e15.8,2i5,1x,'from ceqcp')
	end if
	return
	end
