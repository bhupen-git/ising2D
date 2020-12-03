subroutine swaprandom
  use globaldata
  implicit none
  integer :: sender
  real(8), external :: rn
  integer :: spinbuf(0:N-1),i,flag
 
  type information
    sequence
    real(8) :: Temperature,Energy
  end type information
 
  type(information) :: infobuf,info
  integer :: infotype,oldtypes(1),tosend,torecv
  integer :: blockcounts(1),offsets(1),extent
  
  offsets(1)=0;oldtypes(1)=mpi_double;blockcounts(1)=2
  call mpi_type_struct(1,blockcounts,offsets,oldtypes,infotype,ierror)
  call mpi_type_commit(infotype,ierror)

  info%Temperature=T;info%Energy=E

  if(myrank == 0)then
    sender = rn()*numtasks
    if(sender == 0)then
      flag=0
      do i = 2,numtasks-1
      call mpi_send(flag,1,mpi_int,i,50,mpi_comm_world,ierror)
      end do
      flag=-1
      call mpi_send(flag,1,mpi_int,1,50,mpi_comm_world,ierror)
      call mpi_send(spin,N,mpi_int,1,49,mpi_comm_world,ierror)
      call mpi_send(info,1,infotype,1,51,mpi_comm_world,ierror)
      call mpi_recv(spinbuf,N,mpi_int,1,52,mpi_comm_world,stats,ierror)
      call mpi_recv(infobuf,1,infotype,1,53,mpi_comm_world,stats,ierror)
      info=infobuf
      E=info%Energy;spin=spinbuf
    else
      do i=0, numtasks - 1
      if(i == sender)then
        flag = 1
      else if(i == sender + 1)then
        flag = -1
      else
        flag = 0
      end if
      call mpi_send(flag,1,mpi_int,i,50,mpi_comm_world,ierror)
      end do
    end if
  else
    call mpi_recv(flag,1,mpi_int,0,50,mpi_comm_world,stats,ierror)
    if(flag == 1)then
      call mpi_send(spin,N,mpi_int,myrank+1,49,mpi_comm_world,ierror)
      call mpi_send(info,1,infotype,myrank+1,51,mpi_comm_world,ierror)
      call mpi_recv(spinbuf,N,mpi_int,myrank+1,52,mpi_comm_world,stats,ierror)
      call mpi_recv(infobuf,1,infotype,myrank+1,53,mpi_comm_world,stats,ierror)
      info=infobuf
      E=info%Energy;spin=spinbuf

    else if(flag == -1)then
      call mpi_recv(spinbuf,N,mpi_int,myrank-1,49,mpi_comm_world,stats,ierror)
      call mpi_recv(infobuf,1,infotype,myrank-1,51,mpi_comm_world,stats,ierror)
      if(rn()<exp((E-infobuf%Energy)*(1/T - 1/infobuf%Temperature)))then
        call mpi_send(spin,N,mpi_int,myrank-1,52,mpi_comm_world,ierror)
        call mpi_send(info,1,infotype,myrank-1,53,mpi_comm_world,ierror)
        info=infobuf
        E=info%Energy;spin=spinbuf
      else
        call mpi_send(spinbuf,N,mpi_int,myrank-1,52,mpi_comm_world,ierror)
        call mpi_send(infobuf,1,infotype,myrank-1,53,mpi_comm_world,ierror)
      end if
    
    else
      continue
    end if
  end if
end subroutine


