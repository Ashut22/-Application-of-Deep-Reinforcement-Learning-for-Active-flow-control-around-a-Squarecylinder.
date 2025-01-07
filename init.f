ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	subroutine init 
c
c       initiation of computing data
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
	include 'header'
c
c       write(6,*)'has entered init'
c
	open(7,file='grid.out')
c
	read(7,*)iim,jim,ia,ib,ja,jb
c
	read(7,*)(deltax(i),i=1,iim)
c
	read(7,*)(deltay(j),j=1,jim)
c
	close(7)
c
        open(8,file='daten')
c
        read(8,*)irest,iuprof,iexit,stab,jn1,jnim,
     &		itamax,epsi,stat,beta0,re,uc,al
c
	write(*,*)' irest =',irest,' iuprof =',iuprof,' iexit =',
     &	iexit,' stab =',stab,' iim =',iim,' jim =',
     &	jim,' jn1 =',jn1
     	write(*,*)' jnim =',jnim,' ia =',ia,' ib =',ib,' ja =',ja,
     &	' jb =',jb,' itamax =',itamax,' epsi =',epsi,' stat =',stat
    	write(*,*)' beta0 =',beta0,' re =',re,'uc =',uc,'al =',al
c
	ire=iim-1
	jre=jim-1
	ita=0
	isum=0
c    
	dtmax=1.
c      
	dxmin=1.0
	dymin=1.0
c
	do i=1,iim
	if(deltax(i).le.dxmin)dxmin=deltax(i)
	enddo
c
	do j=1,jim
	if(deltay(j).le.dymin)dymin=deltay(j)
	enddo
c
	write(*,*)'dxmin',dxmin,'dymin',dymin
	deltat=stab*dmin1(dxmin,dymin)
	write(*,*)'deltat',deltat
	rev=1./re
c
	return
	end
