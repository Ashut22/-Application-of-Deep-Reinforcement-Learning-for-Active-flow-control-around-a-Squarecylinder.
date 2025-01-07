cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine otre
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
	write(6,*)'has entered otre'
c
	open(1,file='result1',form='formatted')
	write(1,*)re,ita,zeit,deltat
	write(1,*)iim,jim,ia,ib,ja,jb
	do 190 i=1,iim
	do 190 j=1,jim
        write(1,*)u2(i,j),v2(i,j),p(i,j)
 190 	continue
c
	close(1)
c
	if(ita.gt.27050)then
	open(11,file='res_m',form='formatted')
	write(11,*)re,ita,dt_vel_m,deltat
	write(11,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	write(11,*)um(i,j),vm(i,j),pm(i,j)
	enddo
	enddo
	close (11)
	endif	
c
	if(ita.gt.65000)then
	open(12,file='res_rms',form='formatted')
	write(12,*)re,ita,dt_rms,deltat
	write(12,*)iim,jim,ia,ib,ja,jb
	do i=1,iim
	do j=1,jim
	write(12,*)u2p(i,j),v2p(i,j),uv2p(i,j)
	enddo
	enddo
	close (12)
	endif	
c
c
	open(7,file='open_bn',form='formatted')
	write(7,*)re,ita,zeit,deltat
	write(7,*)iim,jim,ia,ib,ja,jb
	do i=ire-1,ire
	do j=1,jim
	write(7,*)u1(i,j),v1(i,j)
	enddo
	enddo
	close(7)
c
	stop
	end
