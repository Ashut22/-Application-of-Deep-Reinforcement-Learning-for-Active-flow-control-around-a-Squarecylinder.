cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
	program postpro_2d
cmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm
        implicit double precision (a-h,o-z)
        parameter(ix=177,jy=116)
	dimension x(ix),y(jy),p(ix,jy)
	dimension deltax(ix+1),deltay(jy+1),
     1            re_vor(ix,jy),pp(ix,jy)
	common u2(ix,jy),v2(ix,jy)
	common u1(ix,jy),v1(ix,jy)
	common iim,jim,kim,ia,ib,ja,jb
c
	dimension vor_z_temp(ix,jy)
	dimension vor_z(ix,jy)
c
        character*40 infile,outfile
c
        write(*,*)'Give the filename',infile
        read*,infile
c
        open (1,file=infile,form='formatted')
c
	read(1,*)re,ita,zeit,deltat
c	read(1,*)re,ita,zeit,deltat,trans_vel
	read(1,*)iim,jim,ia,ib,ja,jb
	do 200 i=1,iim
	do 200 j=1,jim
	read(1,*)u2(i,j),v2(i,j),p(i,j)
c        u2(i,j)=u2(i,j)/zeit
c        v2(i,j)=v2(i,j)/zeit
c        p(i,j)=p(i,j)/zeit
 200	continue
c
        open(7,file='grid.out')
        read(7,*)iim,jim,ia,ib,ja,jb
        read(7,*)(deltax(i),i=1,iim)
        read(7,*)(deltay(j),j=1,jim)
        close(7)
c
	x(1)=0
        y(1)=0
c
        do i=2,iim-1
        x(i)=x(i-1)+deltax(i)
        enddo
	write(*,*)x(81)
c
        do j=2,jim-1
        y(j)=y(j-1)+deltay(j)
        enddo
	write(*,*)y(jim-1)
c
        call bcc
c
	call bcns
c
	call bcyl2d
c	
	open(99,file='postpro_tecplot.plt',form='formatted')
c
	ire=iim-1
	jre=jim-1
c
c
        do 13 i=2,ire
        do 13 j=2,jre
c
	dx=deltax(i)
	dxr=deltax(i+1)
	dxl=deltax(i-1)
	dx2xr=dx+dxr
	dx2xl=dx+dxl
c
	dy=deltay(j)
	dye=deltay(j+1)
	dyw=deltay(j-1)
	dy2ye=dy+dye
	dy2yw=dy+dyw
c
       	u_r_e=0.5*(u2(i,j+1)+u2(i-1,j+1))
	u_r_w=0.5*(u2(i,j)+u2(i-1,j))
       	ut1_t=u_r_e+(dye/dy2ye)*(u_r_w - u_r_e)
	u_l_e=u_r_w
       	u_l_w=0.5*(u2(i,j-1)+u2(i-1,j-1))
       	ut1_b=u_l_w+(dyw/dy2yw)*(u_l_e - u_l_w)
       	dudy=(ut1_t - ut1_b)/dy
c
	v_e_e=0.5*(v2(i+1,j)+v2(i+1,j-1))
	v_e_w=0.5*(v2(i,j)+v2(i,j-1))
	vt1_e=v_e_e+(dxr/dx2xr)*(v_e_w - v_e_e)
	v_w_e=v_e_w
	v_w_w=0.5*(v2(i-1,j)+v2(i-1,j-1))
	vt1_w=v_w_w+(dxl/dx2xl)*(v_w_e - v_w_w)
	dvdx=(vt1_e-vt1_w)/dx
c
	vor_z_temp(i,j)=dudy-dvdx
c
 13     continue
c 
        do i=1,iim-1
        do j=1,jim-1
c
	dx=deltax(i)
	dxr=deltax(i+1)
	dxl=deltax(i-1)
	dx2xr=dx+dxr
	dx2xl=dx+dxl
c
	dy=deltay(j)
	dye=deltay(j+1)
	dyw=deltay(j-1)
	dy2ye=dy+dye
	dy2yw=dy+dyw
c
        u1(i,j)=u2(i,j+1)+(dye/dy2ye)*(u2(i,j)-u2(i,j+1))
c
        v1(i,j)=v2(i+1,j)+(dxr/dx2xr)*(v2(i,j)-v2(i+1,j))
c
        px_y1_z1=p(i+1,j)+(dxr/dx2xr)*(p(i,j)-p(i+1,j))
        px_y2_z1=p(i+1,j+1)+(dxr/dx2xr)*(p(i,j+1)-p(i+1,j+1))
	pp(i,j)=px_y2_z1+(dye/dy2ye)*(px_y1_z1-px_y2_z1)
c
        enddo
        enddo
c
        write(99,*)'VARIABLES = "X", "Y", "U", "V", "P"'
        write(99,*)'ZONE I=176, J=115, F=BLOCK'
        write(99,22)((x(i),i=1,iim-1,1),j=1,jim-1,1)
        write(99,22)((y(j),i=1,iim-1,1),j=1,jim-1,1)
        write(99,22)((u1(i,j),i=1,iim-1,1),j=1,jim-1,1)
        write(99,22)((v1(i,j),i=1,iim-1,1),j=1,jim-1,1)
        write(99,22)((pp(i,j),i=1,iim-1,1),j=1,jim-1,1)
   22	format(6(f10.4))
c
        close(99)
c
c        write(79,*)'VARIABLES = "X", "Y", "Z", "T"'
c        write(79,*)'ZONE I=141, J=77, K=77, F=BLOCK'
c        write(79,23)(((x(i),i=1,iim-1),j=1,jim-1),k=1,kim-1)
c        write(79,23)(((y(j),i=1,iim-1),j=1,jim-1),k=1,kim-1)
c        write(79,23)(((z(k),i=1,iim-1),j=1,jim-1),k=1,kim-1)
c        write(79,23)(((tt(i,j,k),i=1,iim-1),j=1,jim-1),k=1,kim-1)
c   23	format(6(f10.4))
c        close(79)
c
        end
c**************************************************************
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       subroutine bcc
c
c      boundary conditions for the mass continuity equation
c      conditions for the confining surfaces
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        implicit double precision (a-h,o-z)
        parameter(ix=177,jy=116)
	dimension x(ix),y(jy),p(ix,jy)
	dimension deltax(ix+1),deltay(jy+1),
     1            re_vor(ix,jy),pp(ix,jy)
	common u2(ix,jy),v2(ix,jy)
	common u1(ix,jy),v1(ix,jy)
	common iim,jim,kim,ia,ib,ja,jb
c
	dimension vor_z_temp(ix,jy)
	dimension vor_z(ix,jy)
c
c	write(*,*)'entered bcc'
c
	i=1
	do 50 j=1,jim
	if(irest.eq.1)then
	u2(i,j)=1.0d0
	else
	u2(i,j)=1.0d0+trans_vel
	endif
 50 	continue
c
	j=1
	do 51 i=1,iim
	v2(i,j)=0.0
 51	continue
c
	j=jim
	do 52 i=1,iim
	v2(i,j-1)=0.0
 52 	continue
c
c  ******  obstacle boundary conditions ********
c
	call bcobw
c
         return
	 end
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
        implicit double precision (a-h,o-z)
        parameter(ix=177,jy=116)
	dimension x(ix),y(jy),p(ix,jy)
	dimension deltax(ix+1),deltay(jy+1),
     1            re_vor(ix,jy),pp(ix,jy)
	common u2(ix,jy),v2(ix,jy)
	common u1(ix,jy),v1(ix,jy)
	common iim,jim,kim,ia,ib,ja,jb
c
	dimension vor_z_temp(ix,jy)
	dimension vor_z(ix,jy)
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
	return
	end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
        subroutine bcns 
c   
c       boundary conditions for the navier stokes equations 
c       conditions for the confining surfaces 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        implicit double precision (a-h,o-z)
        parameter(ix=177,jy=116)
	dimension x(ix),y(jy),p(ix,jy)
	dimension deltax(ix+1),deltay(jy+1),
     1            re_vor(ix,jy),pp(ix,jy)
	common u2(ix,jy),v2(ix,jy)
	common u1(ix,jy),v1(ix,jy)
	common iim,jim,kim,ia,ib,ja,jb
c
	dimension vor_z_temp(ix,jy)
	dimension vor_z(ix,jy)
c
c	write(*,*)'entered bcns'
c	
        j=2
	do 170 i=1,iim
	u2(i,j-1)=jn1*u2(i,j)
	v2(i,j-1)=0.0
 170 	continue
c
	j=jim
	do 171 i=1,iim
	u2(i,j)=jnim*u2(i,j-1)
	v2(i,j-1)=0.0
 171	continue
c
	i=1
	do 172 j=1,jim
	u2(i,j)=1.0d0+trans_vel
	v2(i,j)=0.0
 172 	continue
c
c   ********* obstacle boundary conditions *********
c
	call bcobw
c   
c    ***********************************************
	return
	end
c**********************************************************
c
	subroutine bcyl2d
c
c	Obstacle Boundary Conditions For Cylinder
c
c*********************************************************
c
        implicit double precision (a-h,o-z)
        parameter(ix=177,jy=116)
	dimension x(ix),y(jy),p(ix,jy)
	dimension deltax(ix+1),deltay(jy+1),
     1            re_vor(ix,jy),pp(ix,jy)
	common u2(ix,jy),v2(ix,jy)
	common u1(ix,jy),v1(ix,jy)
	common iim,jim,kim,ia,ib,ja,jb
c
	dimension vor_z_temp(ix,jy)
	dimension vor_z(ix,jy)
c
c ************ obstacle boundary  conditions *************
c                

	do i=(ia-1),ib
	j=ja
	    u2(i,j)=-u2(i,j-1)
	enddo
c
	do i=(ia-1),ib
	j=jb
	    u2(i,j)=-u2(i,j+1)
	enddo
c	
	do j=(ja-1),jb
	i=(ia-1)
	v2(i+1,j)=-v2(i,j)
	enddo
c
	do j=(ja-1),jb
	i=(ib+1)
	v2(i-1,j)=-v2(i,j)
	enddo
c
	return
	end
