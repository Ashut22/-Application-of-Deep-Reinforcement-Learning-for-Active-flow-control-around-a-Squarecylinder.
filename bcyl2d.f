c**********************************************************
c
	subroutine bcyl2d
c
c	Obstacle Boundary Conditions For Cylinder
c
c*********************************************************
c
	include 'header'
c
c ************ obstacle boundary  conditions *************
c                

	do i=(ia-1),ib
	j=ja
	    u1(i,j)=-u1(i,j-1)
	enddo
c
	do i=(ia-1),ib
	j=jb
	    u1(i,j)=-u1(i,j+1)
	enddo
c	
	do j=(ja-1),jb
	i=(ia-1)
	v1(i+1,j)=-v1(i,j)
	enddo
c
	do j=(ja-1),jb
	i=(ib+1)
	v1(i-1,j)=-v1(i,j)
	enddo
c
	return
	end

