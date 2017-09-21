	
       subroutine CreateDBox(xi,yi,xf,yf)
       integer xi,yi,xf,yf
       integer fdialog	
       integer nitem,x1,y1,x2,y2
       integer   nDrawX
       integer   nDrawY	
       common /RESWIND/nDrawX,nDrawY
       common /dialog/nitem,fdialog,x1,y1,x2,y2
       fdialog = 1
       nitem = 0
       x1 = xi
       y1 = yi
       x2 = xf
       y2 = yf 
       return	
       end


	subroutine AddLabel(x,y,c,lb)
	integer x,y,c ! c for number of character
	character*80 lb
	character*80 caption(20)
	integer titem(20) ! type of item
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog	 !indicate when DBox is create 
	integer nitem,x1,y1,x2,y2,ident(20)
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	nitem = nitem + 1
	titem(nitem) = 1  !type of item 
	 caption(nitem) = lb
	nchar(nitem) = c
	cx(nitem) = x1+x
	cy(nitem) = y1+y
	endif
	return
	end


	subroutine AddEditText(x,y,c,lb,id)
	integer x,y,id,c ! c for number of character
	character*80 lb
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	integer nitem,x1,y1,x2,y2,ident(20)
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1 ) then
	nitem = nitem + 1
	titem(nitem) = 2  
	caption(nitem) = lb
	nchar(nitem) = c
	cx(nitem) = x1+x
	cy(nitem) = y1+y
	ident(nitem) = id
	endif
	return
	end

	subroutine AddButton(x,y,c,lb,id)
	integer x,y,id,c ! c for number of character
	character*80 lb
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	integer nitem,x1,y1,x2,y2,ident(20)
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	nitem = nitem + 1
	titem(nitem) = 3  
	caption(nitem) = lb
	nchar(nitem) = c
	cx(nitem) = x1+x
	cy(nitem) = y1+y
	ident(nitem) = id
	endif
	return
	end

	subroutine AddRadio(x,y,c,id)
	integer x,y,c,id ! c for number of character
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	character*80 rcaption(20,10)
	integer rcx(20,10),rcy(20,10)
	integer idchoice(20,10),rident(20,10)
	integer nitem,x1,y1,x2,y2,ident(20),nritem(20)
	common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	idchoice(id,1) = 1
	do i=2,10
	idchoice(id,i) = 0
	enddo
	nritem(id) = 0
	nitem = nitem + 1
	titem(nitem) = 4  
	cx(nitem) = x1+x
	cy(nitem) = y1+y
	ident(nitem) = id
	nchar(nitem) = c
	endif
	return
	end


	subroutine AddRadioItem(id,lb)
	integer id ! c for number of character
	character*80 lb
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	integer idchoice(20,10),rident(20,10)
	character*80 rcaption(20,10)
	integer rcx(20,10),rcy(20,10)
	integer nitem,x1,y1,x2,y2,ident(20),nritem(20)
	common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	nritem(id) = nritem(id) + 1
	rcx(id,nritem(id)) = cx(id)+10
	rcy(id,nritem(id)) = cy(id)+(nritem(id)-1)*20+10
	rident(id,nritem(id)) = nritem(id)
	rcaption(id,nritem(id)) = lb
	endif
	return
	end

	subroutine AddCheckBox(x,y,c,id)
	integer x,y,c,id ! c for number of character
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	character*80 rcaption(20,10)
	integer rcx(20,10),rcy(20,10)
	integer idchoice(20,10),rident(20,10)
	integer nitem,x1,y1,x2,y2,ident(20),nritem(20)
	common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	do i=1,10
	idchoice(id,i) = 0
	enddo
	nritem(id) = 0
	nitem = nitem + 1
	titem(nitem) = 5  
	cx(nitem) = x1+x
	cy(nitem) = y1+y
	ident(nitem) = id
	nchar(nitem) = c
	endif
	return
	end

	subroutine AddCheckBoxItem(id,lb)
	integer id ! c for number of character
	character*80 lb
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog
	integer idchoice(20,10),rident(20,10)
	character*80 rcaption(20,10)
	integer rcx(20,10),rcy(20,10)
	integer nitem,x1,y1,x2,y2,ident(20),nritem(20)
	common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
	if ( fdialog .eq. 1) then
	nritem(id) = nritem(id) + 1
	rcx(id,nritem(id)) = cx(id)+10
	rcy(id,nritem(id)) = cy(id)+(nritem(id)-1)*20+10
	rident(id,nritem(id)) = nritem(id)
	rcaption(id,nritem(id)) = lb
	endif
	return
	end


	subroutine ShowDBox(id)
	integer id
	integer idchoice(20,10),rident(20,10),nritem(20)
	integer   nDrawX
        integer   nDrawY	
	common /RESWIND/nDrawX,nDrawY
	character*80 rcaption(20,10)
   	integer rcx(20,10),rcy(20,10)
	common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
	character*80 caption(20)
	integer titem(20)
	integer nchar(20) ! number of character
	integer cx(20)   !coord of x for plot
	integer cy(20)   !coord of y for plot
	integer fdialog  
	integer nitem,x1,y1,x2,y2,ident(20)
	integer flag,kchar,flagkey,flagMouse
        real xmouse,ymouse
 	common /mousekey/flag,xmouse,ymouse,kchar,flagkey,flagMouse
	integer quit,n,prior,i,j
	common /dialog/nitem,fdialog,x1,y1,x2,y2
	common /item/caption,titem,nchar,cx,cy,ident
c
c	
	if ( fdialog .eq. 1 ) then
	call drawbox(x1,y1,x2,y2)
	call drawbox(x1+3,y1+3,x2-3,y2-3)
	do i=1,nitem 
	if (titem(i).eq.1) call ShowLabel(i)
	if (titem(i).eq.2) call ShowEditText(i)
	if (titem(i).eq.3) call ShowButton(i)
	if (titem(i).eq.4) call ShowRadio(i)
	if (titem(i).eq.5) call ShowCheckBox(i)
	end do
	quit = 0
	n = nchar(id)
	prior = id
	flagKey = 0
	flagMouse = 0
	if (id.ne.0) then
      call oneline(' ',1,caption(id),n,cx(id),cy(id)) 
	else
	call xscursr(kchar,xmouse,ymouse)
	endif
	do while(quit .eq. 0)
	if (kchar .eq. 32) then
	flagMouse = 0
	i = 1
	 do while(i .le. nitem)
	  if ( (xmouse>cx(i)) .and. (xmouse<(cx(i)+nchar(i)*10+20)))
     *    then
	   if ((titem(i) .eq. 3) .and. ((ymouse)>cy(i)) .and. 
     *        ((ymouse)<(cy(i)+30))) then
	     quit = 1
	     id = ident(i)
	     goto 20
	   else
	   if ((titem(i) .eq. 2) .and. ((ymouse)>cy(i)) .and. 
     *        ((ymouse)<(cy(i)+25))) then

	     call ShowEditText(prior)
	     prior = i
	 	 n = nchar(i)
		 call oneline(' ',1,caption(i),n,cx(i),cy(i)) 
	   	 i = nitem+1
	   endif
	   if ((titem(i) .eq. 4) .and. ((ymouse)>cy(i)) .and. 
     *        ((ymouse)<(cy(i)+nritem(i)*20))) then

	    call FindRadioItem(i,xmouse,ymouse,j)
		call SetRadioItem(i,j)
	    endif
		if ((titem(i) .eq. 5) .and. ((ymouse)>cy(i)) .and. 
     *        ((ymouse)<(cy(i)+nritem(i)*20))) then
		  call FindCheckBoxItem(i,xmouse,ymouse,j)
	      call SetCheckBoxItem(i,j)

	   endif
	   endif
	endif
	 i = i + 1
	 enddo
	if ((i.eq.nitem+1) .and. (id .ne. 0)) then
	 call oneline(' ',1,caption(prior),n,
     *            cx(prior),cy(prior)) 	  
	else
	call xscursr(kchar,xmouse,ymouse) 
	endif
	else      !KeyDown
	if (kchar .eq. 13) then
	 i = prior+1
	 FlagKey = 0
	 do while((i .le. nitem) .and. (titem(i) .ne. 2))
	 i = i + 1
	 enddo
	 if (i .eq. (nitem+1)) then
	  id = 0
	  goto 20
	 endif 
	 call ShowEditText(prior)
	 if ( i .le. nitem ) then
	   prior = i
	 	 n = nchar(i)
		 call oneline(' ',1,caption(i),n,cx(i),cy(i)) 
		else
	     prior = id
	 	 n = nchar(id)
		 call oneline(' ',1,caption(id),n,cx(id),cy(id)) 
		endif
		else
		call xscursr(kchar,xmouse,ymouse) 
    	endif
		endif
	    
	  enddo
	  endif
20	  call clearwindow(real(x1),real(y1-2),real(x2+10),real(y2))
	  return
	  end

    

		subroutine ShowLabel(i)
    	        integer i
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		call xchars(caption(i),nchar(i),real(cx(i)),real(cy(i))) 
		endif
		return
		end

		subroutine ShowEditText(i)
		integer i
		character*80 caption(20)
		character*80 text
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer ix1,iy1,ix2,iy2
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		ix1=cx(i)
		  iy1=cy(i)
		  ix2=cx(i)+nchar(i)*10+30
		  iy2=cy(i)+25
		call drawbox(ix1,iy1,ix2,iy2)
		do j=1,nchar(i)
		text(1:1) = caption(i)(j:j)
		call xchars(text,1,real(cx(i)+10*j),real(cy(i)+7)) 
		enddo 
		endif
		return
		end

		subroutine ShowButton(i)
		integer i
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer ix1,iy1,ix2,iy2
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		ix1=cx(i)
		  iy1=cy(i)
		  ix2=cx(i)+nchar(i)*10+20
		  iy2=cy(i)+30
		call drawbox(ix1,iy1,ix2,iy2)
		call drawbox(ix1+1,iy1+2,ix2-2,iy2-1)
		call xchars(caption(i),nchar(i),real(cx(i)+10),
     *   real(cy(i)+10)) 
		endif
		return
		end

		subroutine ShowRadio(i)
		integer i
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer idchoice(20,10),rident(20,10)
 		character*80 rcaption(20,10)
		integer rcx(20,10),rcy(20,10),nritem(20)
		common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
		integer ix1,iy1,ix2,iy2
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		ix1=cx(i)
		  iy1=cy(i)
		  ix2=cx(i)+nchar(i)*10+25 
		  iy2=cy(i)+20*nritem(i)
		call drawbox(ix1,iy1,ix2,iy2)
		do j=1,nritem(i)
		call ShowRadioItem(i,j)
		enddo
                call SetRadioItem(i,1)
		endif
		return
		end

		subroutine ShowCheckBox(i)
		integer i
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer idchoice(20,10),rident(20,10)
 		character*80 rcaption(20,10)
		integer rcx(20,10),rcy(20,10),nritem(20)
		common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
		integer ix1,iy1,ix2,iy2
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		ix1=cx(i)
		  iy1=cy(i)
		  ix2=cx(i)+nchar(i)*10+25 
		  iy2=cy(i)+20*nritem(i)
		call drawbox(ix1,iy1,ix2,iy2)
		do j=1,nritem(i)
		call ShowCheckBoxItem(i,j)
		enddo
		endif
		return
		end


		subroutine ShowRadioItem(i,j)
		integer i,j
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer idchoice(20,10),rident(20,10)
 		character*80 rcaption(20,10)
		integer rcx(20,10),rcy(20,10),nritem(20)
		common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
                  call circle(rcx(i,j),rcy(i,j),5)
		call xchars(rcaption(i,j),nchar(i),real(rcx(i,j)+10),
     *          real(rcy(i,j)-7))
		endif
		return
		end

		subroutine ShowCheckBoxItem(i,j)
		integer i,j
		character*80 caption(20)
		integer titem(20)
		integer nchar(20) ! number of character
		integer cx(20)   !coord of x for plot
		integer cy(20)   !coord of y for plot
		integer fdialog
		integer nitem,x1,y1,x2,y2,ident(20)
		integer idchoice(20,10),rident(20,10)
 		character*80 rcaption(20,10)
		integer rcx(20,10),rcy(20,10),nritem(20)
		common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
		common /dialog/nitem,fdialog,x1,y1,x2,y2
		common /item/caption,titem,nchar,cx,cy,ident
		if ( fdialog .eq. 1 ) then
		call drawbox(rcx(i,j)-5,rcy(i,j)-5,rcx(i,j)+5,rcy(i,j)+5)
		call xchars(rcaption(i,j),nchar(i),real(rcx(i,j)+10),
     *          real(rcy(i,j)-7))
		endif
		return
		end

 

		subroutine FindRadioItem(i,xmouse,ymouse,j)
		integer i,j
                real xmouse,ymouse 
 			integer   nDrawX
		  integer   nDrawY	
  		  common /RESWIND/nDrawX,nDrawY

  		  integer idchoice(20,10),rident(20,10)
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  j=0
		  do k=1,nritem(i)
		  if ( (xmouse>(rcx(i,k)-5)) .and. (xmouse<(rcx(i,k)+5))
     *    .and. ((ymouse)>(rcy(i,k)-5)) .and. 	
     *        ((ymouse)<(rcy(i,k)+5))) then
		  j = k
		  return
		  endif
		  enddo
		  return
		  end

		subroutine FindCheckBoxItem(i,xmouse,ymouse,j)
		integer i,j
                real xmouse,ymouse 
 		integer   nDrawX
		  integer   nDrawY	
  		common /RESWIND/nDrawX,nDrawY

  		  integer idchoice(20,10),rident(20,10)
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  j=0
		  do k=1,nritem(i)
		  if ( (xmouse>(rcx(i,k)-5)) .and. (xmouse<(rcx(i,k)+5))
     *    .and. ((ymouse)>(rcy(i,k)-5)) .and.  
     *        ((ymouse)<(rcy(i,k)+5))) then
		  j = k
		  return
		  endif
		  enddo
		  return
		  end



		  subroutine SetRadioItem(i,k)
		  integer i,k
		  !interface
		  !integer function GetRadioItem(l)
		  !integer  l
		  !end function
		  !end interface
  		  integer idchoice(20,10),rident(20,10)
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  if (k.eq.0) return
		  j = k
		  call fillcircle(rcx(i,j),rcy(i,j),5)
		  call GetRadioItem(i,j)
		  if (j.ne.k) then
		  call xset_color(5)
		  call fillcircle(rcx(i,j),rcy(i,j),5)
		  call xset_color(1)
		  call circle(rcx(i,j),rcy(i,j),5)
		  idchoice(i,j) = 0
		  endif
		  idchoice(i,k) = 1
		  return
		  end

		 subroutine GetRadioItem(l,j)
		  integer  l,j
  		  integer idchoice(20,10),rident(20,10),k
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  j = 0
		  do k=1,10
		  if (idchoice(l,k).ne.0) j = k
		  enddo
		  return 
		  end

		subroutine CheckBoxItem(i,k,flag)
		  integer i,k,flag
  		  integer idchoice(20,10),rident(20,10)
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  if (idchoice(i,k).eq.0) then
		  flag =  0
		  else
		  flag =  1
		  endif 
                  return
		  end


		  subroutine SetCheckBoxItem(i,k)
		  integer i,k
  		  integer idchoice(20,10),rident(20,10)
  		  character*80 rcaption(20,10)
		  integer rcx(20,10),rcy(20,10),nritem(20)
		  common /Radio/rcaption,idchoice,nritem,rident,rcx,rcy
		  if (k.eq.0) return
		  j = k
		  idchoice(i,j) = 1 - idchoice(i,j)
                  if (idchoice(i,j) .eq. 1) then
		  call xset_color(1)
		  call fillbox(real(rcx(i,j)-5),real(rcy(i,j)-5),
     *            real(rcx(i,j)+5),real(rcy(i,j)+5))
                  else
		  call xset_color(5)
		  call fillbox(real(rcx(i,j)-5),real(rcy(i,j)-5),
     *            real(rcx(i,j)+5),real(rcy(i,j)+5))
		  call xset_color(1)
		  call drawbox(rcx(i,j)-5,rcy(i,j)-5,rcx(i,j)+5,rcy(i,j)+5)
                  endif
		  return
		  end

		  subroutine GetEditText(i,answer)
		  integer i
		  character*80 answer
		  character*80 caption(20)
		  integer titem(20)
		  integer nchar(20) ! number of character
		  integer cx(20)   !coord of x for plot
		  integer cy(20)   !coord of y for plot
		  integer fdialog
		  integer nitem,x1,y1,x2,y2,ident(20)
		  integer j
		  common /dialog/nitem,fdialog,x1,y1,x2,y2
		  common /item/caption,titem,nchar,cx,cy,ident
		  answer = ''
		  j = 1
		  if ( fdialog .eq. 1 ) then
		   do while( (ident(j) .ne. i) .and. (j.le.nitem) )
		    j = j+1
		   enddo
		   if (j.le.nitem) answer = caption(j)
		  endif
		  return
		  end
