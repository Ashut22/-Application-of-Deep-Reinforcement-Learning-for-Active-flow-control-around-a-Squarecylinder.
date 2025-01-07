cssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
c
      subroutine nseqcp
c
c     navier-stokes equations for constant properties
c                                              1994.12.12
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        include 'header'
c
c      write(6,*)'has entered nseqcp'   
c
	zeit=zeit+deltat
c
	do 150 i=2,ire-1
	do 150 j=2,jre
c
	dxr=deltax(i+1)
	dx=deltax(i)
	dxl=deltax(i-1)
c
	dyt=deltay(j+1)
	dy=deltay(j)
	dyb=deltay(j-1)
c
cccccccccccccccccc  diff-u     ccccccccccccccccccccccccccccccc
c     duu / dx
        u1a = u1(i-1,j)  +  u1(i,j)
        u22 = u1(i-1,j)  -  u1(i,j)
        u3  = u1(i,j)    +  u1(i+1,j)
        u4  = u1(i,j)    -  u1(i+1,j)
c
c     duv  / dy
        u5 = u1(i,j-1)   +  u1(i,j)
        u6 = u1(i,j-1)   -  u1(i,j)
        u7 = u1(i,j)     +  u1(i,j+1)
        u8 = u1(i,j)     -  u1(i,j+1)
c
c     duv/dx
        u13 = u1(i-1,j)  + u1(i-1,j+1)
        u14 = u7
c
cccccccccccccccccccccc    diff -v    ccccccccccccccccccccccc
c     dvu / dx
        v1a = v1(i,j-1)   + v1(i+1,j-1)     
        v22 = v1(i,j)     + v1(i+1,j) 
c
c     duv / dx
        v3 = v1(i-1,j)    + v1(i,j)
        v4 = v1(i-1,j)    - v1(i,j)
        v5 = v1(i,j)      + v1(i+1,j)
        v6 = v1(i,j)      - v1(i+1,j)
c
c     dvv / dy
        v7 = v1(i,j-1)    + v1(i,j)
        v8 = v1(i,j-1)    - v1(i,j)
        v9 = v1(i,j)      + v1(i,j+1)
        v10= v1(i,j)      - v1(i,j+1)  
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      dpdx = (p(i,j) - p(i+1,j))/(0.5*(dxr+dx))
c
      dpdy = (p(i,j) - p(i,j+1))/(0.5*(dyt+dy))
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	dx2xr=dx+dxr
	dx2xl=dx+dxl
	dy2yt=dy+dyt
	dy2yb=dy+dyb
c
c ********************** U - Momentum ****************************
c
c	
	vu_e=v1(i+1,j)+(dxr/(dx2xr))*(v1(i,j)-v1(i+1,j))
	vu_w=v1(i+1,j-1)+(dxr/(dx2xr))*(v1(i,j-1)-v1(i+1,j-1))
	v_in_um=0.5*(vu_e+vu_w)
c
cccccccccccccc---First Order Upwinding ----ccccccccccccccccccccccc
c
	r1x=deltax(i)/deltax(i+1)
	r1n=0.5*(deltay(j)+deltay(j-1))
	r1d=0.5*(deltay(j)+deltay(j+1))
	r1y=r1n/r1d
c
	duuwdx=(u1(i,j)/deltax(i+1))*((-1.0/(r1x*(r1x+1.0)))*
     1	        u1(i-1,j)-(1.0-1.0/r1x)*u1(i,j)+
     2	        (r1x/(r1x+1.0))*u1(i+1,j))-al*dabs(u1(i,j))*
     3		(1.0/(2.0*deltax(i+1)))*(2.0/(r1x*(r1x+1.0))*
     4		u1(i-1,j)-2.0/r1x*u1(i,j)+2.0/(r1x+1.0)*
     5		u1(i+1,j))
c
	dvuwdy=(v_in_um/r1d)*((-1.0/(r1y*(r1y+1.0)))*
     1	        u1(i,j-1)-(1.0-1.0/r1y)*u1(i,j)+
     2	        (r1y/(r1y+1.0))*u1(i,j+1))-al*dabs(v_in_um)*
     3		(1.0/(2.0*r1d))*(2.0/(r1y*(r1y+1.0))*
     4		u1(i,j-1)-2.0/r1y*u1(i,j)+2.0/(r1y+1.0)*
     5		u1(i,j+1))
c
cccccccccccccc---Central Differencing ----ccccccccccccccccccccccc
c
        uu_e=u1(i,j+1)+(dyt/(dy2yt))*(u1(i,j)-u1(i,j+1))
        uu_w=u1(i,j-1)+(dyb/(dy2yb))*(u1(i,j)-u1(i,j-1))
        vu_e=v1(i+1,j)+(dxr/(dx2xr))*(v1(i,j)-v1(i+1,j))
        vu_w=v1(i+1,j-1)+(dxr/(dx2xr))*(v1(i,j-1)-v1(i+1,j-1))
c
        duucdx=(1.0/(2.0*dx2xr))*(u3**2.0-u1a**2.0)
        dvucdy=(1.0/dy)*(uu_e*vu_e-uu_w*vu_w)
c
      	duudx=duucdx
      	dvudy=dvucdy
c
	d2udx2=(2.0/dx2xr)*((-u4/dxr)+(u22/dx))
	d2udy2=(2.0/dy)*((-u8/dy2yt)+(u6/dy2yb))
c
        xtt=rev*(d2udx2+d2udy2)
c
        residu=(-duudx-dvudy+xtt)
        if(ita.eq.1 .or. irest.eq.1)then
        u2(i,j)=u1(i,j)+deltat*(residu+dpdx)
        else
        u2(i,j)=u1(i,j)+deltat*(0.5*(3.0*residu-residu_0(i,j))
     $        +dpdx)
        endif
        residu_0(i,j)=residu
c
c	ve=v1(i+1,j)+(dxr/(dx2xr))*(v1(i,j)-v1(i+1,j))
c	vw=v1(i-1,j)+(dxl/(dx2xl))*(v1(i,j)-v1(i-1,j))
c	ue=u1(i,j+1)+(dyt/(dy2yt))*(u1(i,j)-u1(i,j+1))
c	uw=u1(i-1,j+1)+(dyt/(dy2yt))*(u1(i-1,j)-u1(i-1,j+1))
c
c ********************** V - Momentum ****************************
c
	uw_e=u1(i,j+1)+(dyt/(dy2yt))*(u1(i,j)-u1(i,j+1))
	uw_w=u1(i-1,j+1)+(dyt/(dy2yt))*(u1(i-1,j)-u1(i-1,j+1))
	u_in_vm=0.5*(uw_e+uw_w)
c	
cccccccccccccc---First Order Upwinding ----ccccccccccccccccccccccc
c
	r1n=0.5*(deltax(i)+deltax(i-1))
	r1d=0.5*(deltax(i)+deltax(i+1))
	r1x=r1n/r1d
	r1y=deltay(j)/deltay(j+1)
c
	duvwdx=(u_in_vm/r1d)*((-1.0/(r1x*(r1x+1.0)))*
     1	        v1(i-1,j)-(1.0-1.0/r1x)*v1(i,j)+
     2	        (r1x/(r1x+1.0))*v1(i+1,j))-al*dabs(u_in_vm)*
     3		(1.0/(2.0*r1d))*(2.0/(r1x*(r1x+1.0))*
     4		v1(i-1,j)-2.0/r1x*v1(i,j)+2.0/(r1x+1.0)*
     5		v1(i+1,j))
c
	dvvwdy=(v1(i,j)/deltay(j+1))*((-1.0/(r1y*(r1y+1.0)))*
     1	        v1(i,j-1)-(1.0-1.0/r1y)*v1(i,j)+
     2	        (r1y/(r1y+1.0))*v1(i,j+1))-al*dabs(v1(i,j))*
     3		(1.0/(2.0*deltay(j+1)))*(2.0/(r1y*(r1y+1.0))*
     4		v1(i,j-1)-2.0/r1y*v1(i,j)+2.0/(r1y+1.0)*
     5		v1(i,j+1))
c
cccccccccccccc---Central Differencing ----ccccccccccccccccccccccc
c
        uv_e=u1(i,j+1)+(dyt/(dy2yt))*(u1(i,j)-u1(i,j+1))
        uv_w=u1(i-1,j+1)+(dyt/(dy2yt))*(u1(i-1,j)-u1(i-1,j+1))
        vv_e=v1(i+1,j)+(dxr/(dx2xr))*(v1(i,j)-v1(i+1,j))
        vv_w=v1(i-1,j)+(dxl/(dx2xl))*(v1(i,j)-v1(i-1,j))
c
        duvcdx=(1.0/dx)*(uv_e*vv_e-uv_w*vv_w)
        dvvcdy=(1.0/(2.0*dy2yt))*(v9**2.0-v7**2.0)
c
      	duvdx=duvcdx
      	dvvdy=dvvcdy
c
	d2vdx2=(2.0/dx)*((-v6/dx2xr)+(v4/dx2xl))
	d2vdy2=(2.0/dy2yt)*((-v10/dyt)+(v8/dy))
c
        ytt = rev*(d2vdx2 + d2vdy2 )
c
        residv =(-duvdx-dvvdy+ytt)
        if(ita.eq.1 .or. irest.eq.1)then
        v2(i,j)=v1(i,j)+deltat*(residv+dpdy)
        else
        v2(i,j)=v1(i,j)+deltat*(0.5*(3.0*residv-residv_0(i,j))
     $         +dpdy)
        endif
        residv_0(i,j)=residv
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
 150   continue
c
c      write(6,*) 'leaving nseqcp '
c
      return
      end
c
cssssssssssssssssssssssssssssssssssssssssssssssssssssssss
