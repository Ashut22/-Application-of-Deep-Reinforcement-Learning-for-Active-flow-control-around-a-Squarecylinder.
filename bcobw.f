ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
         subroutine bcobw
c
c      obstacle-boundary-conditions for bcc,bcns
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  ****** boundary conditions for the obstacle ******
c
	include 'header'
c
	do 60 i=(ia-1),ib
	do 60 j=ja,jb
	u1(i,j)=0.
	u2(i,j)=0.
 60	continue
c
	do 61 i=ia,ib
	do 61 j=(ja-1),jb
	v1(i,j)=0.
	v2(i,j)=0.
 61     continue
      

      v1(ib,jb+1)=0.1
      v2(ib,jb+1)=0.1
      v1(ib,ja-2) =-0.1
      v2(ib,ja-2)=-0.1
      
	return
	end
