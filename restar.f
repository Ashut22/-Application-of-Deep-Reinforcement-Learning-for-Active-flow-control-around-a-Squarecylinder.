ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine restar
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c	write(6,*) 'has entered restar'
c
        open (1,file='result1',form='formatted')
	read(1,*)re,ita,zeit,deltat
	read(1,*)iim,jim,ia,ib,ja,jb
	do 200 i=1,iim
	do 200 j=1,jim
	read(1,*)u2(i,j),v2(i,j),p(i,j)
	u1(i,j)=u2(i,j)
	v1(i,j)=v2(i,j)
 200	continue
	close (1)
c
c	open(7,file='open_bn',form='formatted')
c	read(7,*)re,ita,zeit,deltat
c	read(7,*)iim,jim,ia,ib,ja,jb
c	do i=ire-1,ire
cc	do j=1,jim
c	read(7,*)u1(i,j),v1(i,j)
c	enddo
c	enddo
c	close(7)
        ifile=0
c
	i=1
	do 202 j=1,jim
	qinlet(j)=u2(i,j)
 202    continue
c
c	if(ita.gt.27050)then
c       open (11,file='res_m',form='formatted')
c	read(11,*)re,ita,dt_vel_m,deltat
c	read(11,*)iim,jim,ia,ib,ja,jb
c	do i=1,iim
c	do j=1,jim
c	read(11,*)um(i,j),vm(i,j),pm(i,j)
c	write(*,*)i,j,um(i,j),vm(i,j),pm(i,j)
c	umi(i,j)=um(i,j)/dt_vel_m
c	vmi(i,j)=vm(i,j)/dt_vel_m
c	enddo
c	enddo
c	close (11)
c	else
c	dt_vel_m=0.0
c	do i=1,iim
c	do j=1,jim
c	um(i,j)=0.0
c	vm(i,j)=0.0
c	pm(i,j)=0.0
c	enddo
c	enddo
c	endif	
c
c	if(ita.gt.65000)then
c       open (12,file='res_rms',form='formatted')
cc	read(12,*)re,ita,dt_rms,deltat
c	read(12,*)iim,jim,ia,ib,ja,jb
c	do i=1,iim
c	do j=1,jim
c	read(12,*)u2p(i,j),v2p(i,j),uv2p(i,j)
c	enddo
c	enddo
c	close (12)
c	else
c	dt_rm=0.0
c	do i=1,iim
c	do j=1,jim
c	u2p(i,j)=0.0
cc	v2p(i,j)=0.0
c	uv2p(i,j)=0.0
c	enddo
c	enddo
c	endif	
c
c	write(*,*)'leaving restar'
c
	return
	end
