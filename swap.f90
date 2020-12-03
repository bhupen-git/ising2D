subroutine swapreps(commblock)
  use globaldata
  implicit none
  real(8), external :: rn
  integer, external :: location
  integer, intent(in) :: commblock

  type information
    sequence
    real(8) :: Temperature,Energy
    integer :: swprnk!,accept
  end type information

  type(information) :: infobuf,info
  integer :: infotype,oldtypes(2),tosend,torecv
  integer :: blockcounts(2),offsets(2),extent

  offsets(1)=0;oldtypes(1)=mpi_double;blockcounts(1)=2
  call mpi_type_extent(mpi_double,extent,ierror)
  offsets(2)=2*extent;oldtypes(2)=mpi_int;blockcounts(2)=1
  call mpi_type_struct(2,blockcounts,offsets,oldtypes,infotype,ierror)
  call mpi_type_commit(infotype,ierror)

  info%swprnk=sub_s;info%Temperature=T;info%Energy=E

  if(commblock==1)then
    if(modulo(collect(myrank),2)==0)then
      tosend=location(collect,sub_s + 1,numtasks-1)
      call mpi_send(info,1,infotype,tosend,50,mpi_comm_world,ierror)
      call mpi_recv(infobuf,1,infotype,tosend,51,mpi_comm_world,stats,ierror)
      info=infobuf
      sub_s=info%swprnk;T=info%Temperature
    else
      torecv=location(collect,sub_s-1,numtasks-1)
      call mpi_recv(infobuf,1,infotype,torecv,50,mpi_comm_world,stats,ierror)
      if(rn()<exp((E-infobuf%Energy)*(1/T - 1/infobuf%Temperature)))then
      !if(rn() < 1)then
        call mpi_send(info,1,infotype,torecv,51,mpi_comm_world,ierror)
        info=infobuf
        sub_s=info%swprnk;T=info%Temperature
      else
        call mpi_send(infobuf,1,infotype,torecv,51,mpi_comm_world,ierror)
      end if
    end if
  else
    if(modulo(collect(myrank),2)/=0)then
      if(collect(myrank)/= numtasks-1)then
        tosend=location(collect,sub_s+1,numtasks-1)
        call mpi_send(info,1,infotype,tosend,52,mpi_comm_world,ierror)
        call mpi_recv(infobuf,1,infotype,tosend,53,mpi_comm_world,stats,ierror)
        info=infobuf
        sub_s=info%swprnk;T=info%Temperature
      end if
    else
      if(collect(myrank)/=0)then
        torecv=location(collect,sub_s-1,numtasks-1)
        call mpi_recv(infobuf,1,infotype,torecv,52,mpi_comm_world,stats,ierror)
        if(rn()<exp((E-infobuf%Energy)*(1/T - 1/infobuf%Temperature)))then
        !if(rn() < 1)then
          call mpi_send(info,1,infotype,torecv,53,mpi_comm_world,ierror)
          info=infobuf
          sub_s=info%swprnk;T=info%Temperature
        else
          call mpi_send(infobuf,1,infotype,torecv,53,mpi_comm_world,ierror)
        end if
      end if
    end if
  end if
end subroutine swapreps

integer function location(array,val,endloc)
  implicit none
  integer :: val,i,endloc
  integer :: array(0:endloc)
  do i=0,endloc
    if(array(i)==val)then
      location=i
      exit
    end if
  end do
end function location


