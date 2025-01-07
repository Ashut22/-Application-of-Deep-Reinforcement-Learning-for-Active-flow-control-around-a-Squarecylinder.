ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
	subroutine otpt
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c     
c        write(6,*) 'has entered otpt'
c	
c	Writing the v-velocity 
c
	open(2,file='frea1000',access='append')
	write(2,24)zeit,v2(87,81)
	close(2)
 24	format(2x,f10.5,2x,f10.5)
c
	dxl_1=deltax(ia-1)
	dxl_2=deltax(ia-2)
	dxr_1=deltax(ib+1)
	dxr_2=deltax(ib+2)
c
	dyb_1=deltay(ja-1)
	dyb_2=deltay(ja-2)
	dyt_1=deltay(jb+1)
	dyt_2=deltay(jb+2)
c
	tt_yt_1=dyt_2**2.0/4.0+dyt_1*dyt_2+0.75*dyt_1**2.0
	tt_yt_2=dyt_1+0.5*dyt_2
	tt_yt_3=0.5*(dyt_1+dyt_2)
c
	tt_yb_1=dyb_2**2.0/4.0+dyb_1*dyb_2+0.75*dyb_1**2.0
	tt_yb_2=dyb_1+0.5*dyb_2
	tt_yb_3=0.5*(dyb_1+dyb_2)
c
	tt_xr_1=dxr_2**2.0/4.0+dxr_1*dxr_2+0.75*dxr_1**2.0
	tt_xr_2=dxr_1+0.5*dxr_2
	tt_xr_3=0.5*(dxr_1+dxr_2)
c
	tt_xl_1=dxl_2**2.0/4.0+dxl_1*dxl_2+0.75*dxl_1**2.0
	tt_xl_2=dxl_1+0.5*dxl_2
	tt_xl_3=0.5*(dxl_1+dxl_2)
c
	cpsum=0.0
	do 192 i=ia,ib
	q1=(1.0/re)*(dyb_2*0.5+dyb_1)*(0.5*dyb_1)/tt_yb_1
	q2=(v2(i,ja-1)/dyb_1-(v2(i,ja-2)-v2(i,ja-1))/dyb_2)
	q3=(tt_yb_2**2.0)*p(i,ja-1)/tt_yb_1
	q4=((dyb_1*0.5)**2.0)*p(i,ja-2)/tt_yb_1
	ppb=q1*q2+q3-q4
c
	q5=(1.0/re)*(dyt_2*0.5+dyt_1)*(0.5*dyt_1)/tt_yt_1
	q6=(v2(i,jb+1)/dyt_1-(v2(i,jb+2)-v2(i,jb+1))/dyt_2)
	q7=(tt_yt_2**2.0)*p(i,jb+1)/tt_yt_1
	q8=((dyt_1*0.5)**2.0)*p(i,jb+2)/tt_yt_1
	ppt=q5*q6+q7-q8
	cpsum=cpsum+2.0*(ppb-ppt)
 192	continue
	cp1=cpsum/(ib-ia+1)
c
	sl=0.0
	sr=0.0
	cf1sum=0.0
	do 193 j=ja,jb
	y1=v2(ia-1,j)*(dxl_1+0.5*dxl_2)**2.0
	y2=v2(ia-2,j)*(dxl_1*0.5)**2.0
	sl=(y1-y2)/((dxl_1+dxl_2*0.5)*(dxl_1*0.5)*tt_xl_3)
	y11=v2(ib+1,j)*(dxr_1+0.5*dxr_2)**2.0
	y12=v2(ib+2,j)*(dxr_1*0.5)**2.0
	sr=(y11-y12)/((dxr_1+dxr_2*0.5)*(dxr_1*0.5)*tt_xr_3)
	cf1sum=cf1sum+(2.0/re)*(sl+sr)
 193	continue
c
	cf1=cf1sum/(jb-ja+1)
c	cl=cp1+cf1
c
	open(3,file='cl1000',form='formatted',access='append')
	write(3,22)zeit,cp1,cf1
	close(3)
c
	cdsum=0.0
	do 194 j=ja,jb
	q11=(1.0/re)*(dxl_2*0.5+dxl_1)*(0.5*dxl_1)/tt_xl_1
	q12=(u2(ia-1,j)/dxl_1-(u2(ia-2,j)-u2(ia-1,j))/dxl_2)
	q13=(tt_xl_2**2.0)*p(ia-1,j)/tt_xl_1
	q14=((dxl_1*0.5)**2.0)*p(ia-2,j)/tt_xl_1
	ppl=q11*q12+q13-q14
c
	q15=(1.0/re)*(dxr_2*0.5+dxr_1)*(0.5*dxr_1)/tt_xr_1
	q16=(u2(ib+1,j)/dxr_1-(u2(ib+2,j)-u2(ib+1,j))/dxr_2)
	q17=(tt_xr_2**2.0)*p(ib+1,j)/tt_xr_1
	q18=((dxr_1*0.5)**2.0)*p(ib+2,j)/tt_xr_1
	ppr=q15*q16+q17-q18
	cdsum=cdsum+2.0*(ppl-ppr)
c
 194	continue
c
	cd1=cdsum/(jb-ja+1)
c
	sb=0.0
	st=0.0
	cf2sum=0.0
	do 195 i=ia,ib
	x1=u2(i,ja-1)*(dyb_1+0.5*dyb_2)**2.0
	x2=u2(i,ja-2)*(dyb_1*0.5)**2.0
	sb=(x1-x2)/((dyb_1+dyb_2*0.5)*(dyb_1*0.5)*tt_yb_3)
	x11=u2(i,jb+1)*(dyt_1+0.5*dyt_2)**2.0
	x12=u2(i,jb+2)*(dyt_1*0.5)**2.0
	st=(x11-x12)/((dyt_1+dyt_2*0.5)*(dyt_1*0.5)*tt_yt_3)
	cf2sum=cf2sum+(2.0/re)*(sb+st)
 195	continue
c
	cf2=cf2sum/(ib-ia+1)
c	cd=cd1+cf2
c
	open(4,file='cd1000',form='formatted',access='append')
	write(4,22)zeit,cd1,cf2
	close(4)
 22	format(1x,f14.5,2(2x,f14.9))
c
c	if(cl2.lt.0.0 .and. cp1.gt.0.0)then
c	write(1)re,ita,zeit,deltat
c	write(1)iim,jim,kim,ia,ib,ka,kb
c	write(1)u2,v2,w2,p
c	close(1)
c	endif
c
        write(*,*)'ifile=',ifile
        if(ifile.eq.0)then
	if((cl1-cl2).lt.0.0 .and. (cl2-cp1).gt.0.0 .and. cp1.gt.0.1)then
  	open (3, file='result_high_cl',form='formatted')
	write(3,*)re,ita,zeit,deltat
	write(3,*)iim,jim,ia,ib,ja,jb
	do 198 i=1,iim
	do 198 j=1,jim
	write(3,*)u2(i,j),v2(i,j),p(i,j)
 198	continue
	close(3)
	endif
        ifile=ifile+1
	endif
cc
c	if(cl2.gt.0.0 .and. cp1.lt.0.0)then
c	write(3)re,ita,zeit,deltat
c	write(3)iim,jim,kim,ia,ib,ka,kb
c	write(3)u2,v2,w2,p
c	close(3)
c	endif
cc
c	if((cl1-cl2).gt.0.0 .and. (cl2-cp1).lt.0.0 .and. cp1.lt.-1.0)then
c	write(4)re,ita,zeit,deltat
c	write(4)iim,jim,kim,ia,ib,ka,kb
c	write(4)u2,v2,w2,p
c	close(4)
c	endif
cc
	cl1=cl2
	cl2=cp1
c
c	Writing the v-velocity 
c
c	************** Writing u- and v- for phase plot **********
c
	lk=0
	do ll=1,7
	i=101+lk
	uip(ll)=0.5*(u2(i,60)+u2(i,61))
	vip(ll)=0.5*(v2(i,61)+v2(i+1,61))

	lk=15*ll
        enddo

        open(27,file='phase1',access='append')  
	write(27,21)zeit,uip(1),vip(1)
	close(27)

c        open(37,file='phase2',access='append')  
c	write(37,21)zeit,uip(2),vip(2)
c	close(37)
c
c        open(47,file='phase3',access='append')  
c	write(47,21)zeit,uip(3),vip(3)
c	close(47)
c
c        open(57,file='phase4',access='append')  
c	write(57,21)zeit,uip(4),vip(4)
c	close(57)
c
c        open(67,file='phase5',access='append')  
c	write(67,21)zeit,uip(5),vip(5)
c	close(67)
c
c        open(77,file='phase6',access='append')  
c	write(77,21)zeit,uip(6),vip(6)
c	close(77)
c
c        open(87,file='phase7',access='append')  
cc	write(87,21)zeit,uip(7),vip(7)
c	close(87)
c
 21	format(1x,f9.5,2(2x,f14.9))
c
c	if(ita.gt.27050)then
c	dt_vel_m=dt_vel_m+deltat
c	do i=1,iim
c	do j=1,jim
c	um(i,j)=um(i,j)+deltat*u2(i,j)
c	vm(i,j)=vm(i,j)+deltat*v2(i,j)
c	pm(i,j)=pm(i,j)+deltat*p(i,j)
c	enddo
c	enddo
c	endif
cc
c	if(ita.gt.65000)then
c	dt_rms=dt_rms+deltat
c	do i=1,iim
c	do j=1,jim
c	if(ita.eq.ita/150*150)then
c	umi(i,j)=um(i,j)/dt_vel_m
c	vmi(i,j)=vm(i,j)/dt_vel_m
c	endif
c	u2p(i,j)=u2p(i,j)+deltat*(u2(i,j)-umi(i,j))**2.0
c	v2p(i,j)=v2p(i,j)+deltat*(v2(i,j)-vmi(i,j))**2.0
c	uv2p(i,j)=uv2p(i,j)+deltat*((0.5*(v2(i,j)+v2(i,j-1)))-
c    1  (0.5*(vmi(i,j)+vmi(i,j-1))))*((0.5*(u2(i,j)+u2(i-1,j)))-
c   2  (0.5*(umi(i,j)+umi(i-1,j))))
c	enddo
c	enddo
c	endif
c
	if(ita.eq.ita/25*25.or.iwrite.eq.1)goto 777
        if(dtmax.le.stat .or. ita.ge.itamax)goto 777 
	goto 888
  777	open (1, file='result1',form='formatted')
	write(1,*)re,ita,zeit,deltat
	write(1,*)iim,jim,ia,ib,ja,jb
	do 197 i=1,iim
	do 197 j=1,jim
	write(1,*)u2(i,j),v2(i,j),p(i,j)
 197	continue
	close(1)
c
c	if(ita.gt.27050)then
c	open(11,file='res_m',form='formatted')
c	write(11,*)re,ita,dt_vel_m,deltat
c	write(11,*)iim,jim,ia,ib,ja,jb
c	do i=1,iim
c	do j=1,jim
c	write(11,*)um(i,j),vm(i,j),pm(i,j)
c	enddo
c	enddo
c	close (11)
c	endif	
c
c	if(ita.gt.65000)then
c	open(12,file='res_rms',form='formatted')
c	write(12,*)re,ita,dt_rms,deltat
c	write(12,*)iim,jim,ia,ib,ja,jb
c	do i=1,iim
c	do j=1,jim
cc	write(12,*)u2p(i,j),v2p(i,j),uv2p(i,j)
c	enddo
c	enddo
c	close (12)
c	endif	
c
c	open(7,file='open_bn',form='formatted')
c	write(7,*)re,ita,zeit,deltat
c	write(7,*)iim,jim,ia,ib,ja,jb
c	do i=ire-1,ire
c	do j=1,jim
c	write(7,*)u1(i,j),v1(i,j)
c	enddo
c	enddo
c	close(7)
c
 888    if(dtmax.le.stat .or. ita.ge.itamax) then
	stop
	endif
	return
	end
