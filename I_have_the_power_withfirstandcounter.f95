!this thing is intended to recursively give the positive root of a number to whatever power
!we should end up with beaver=independant**(1/root)
!this will also show the steps involved in the process
!note: this works because the root of x will always be smaller than x for x>1, root>1
!in order to make this more general, special cases have to be coded in
recursive function rooted(independant,root,arrange_min,arrange_max,stupid_counter) result(beaver)
	real*8, intent(in) :: independant, root
	real*8 :: beaver, arrange_min, arrange_max
	integer :: stupid_counter
	character(len=29) :: formatt
	formatt='(A,F0.6,A,F0.6,A,F0.6,A,I0,A)'
	!find the mid-point in the range from arrange_min to arrange_max
	beaver=((arrange_max-arrange_min)/2)+arrange_min
	!the following conditions trip; if the answer is found, the range is too small or we go on too long
	if ((beaver**root==independant) .or. (arrange_min==arrange_max) .or. (stupid_counter==1000)) then
		print formatt,'arrange_min=',arrange_min,'  arrange_max=', &
		arrange_max,'  beaver=',beaver,'  stupid_counter=',stupid_counter,'  converge conditionally'	
	else		
		stupid_counter=stupid_counter+1
		!we split the range into two partitions. The rooted beaver being higher or lower than the independant
		!decides wether the next recursive step moves into the higher or lower partition by putting beaver in
		!either the arrange_min or arrange_max positions. Because the rooted beaver should equal the independant
		!when the beaver has a root, the beaver should converge on the correct value 
		if (beaver**root>independant) then
			print formatt,'arrange_min=',arrange_min, &
			'  arrange_max=',arrange_max,'  beaver=',beaver,'  stupid_counter=',stupid_counter,'  operate down'
			beaver=rooted(independant,root,arrange_min,beaver,stupid_counter)
		else if (beaver**root<independant) then
			print formatt,'arrange_min=',arrange_min, &
			'  arrange_max=',arrange_max,'  beaver=',beaver,'  stupid_counter=',stupid_counter,'  operate up'
			beaver=rooted(independant,root,beaver,arrange_max,stupid_counter)
		else
			print formatt,'arrange_min=',arrange_min, &
			'  arrange_max=',arrange_max,'  beaver=',beaver,'  stupid_counter=',stupid_counter,'  coverge implicitly'
		end if
	end if
print formatt,'arrange_min=',arrange_min,'  arrange_max=', &
arrange_max,'  beaver=',beaver,'  stupid_counter=',stupid_counter,'  end'				
end function rooted

		
!put the range for each recursive step in the program, not the function
program I_have_the_power
implicit none

real*8 :: x,z,y,rooted,hole=0.
integer :: begin=0

print*,'This will find the value y for the equation y=x^(1/z) for x>0 (complex numbers coming)'
print*,''
print*,'Enter your x'
read(*,*) x
print*,'Enter your z'
read(*,*) z

y=rooted(x,z,hole,x,begin)

print*,'x=',x
print*,'z=',z
print*,'y=',y

end program
