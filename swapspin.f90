subroutine swapspin(commblock)
  use globaldata
  implicit none
  real(8), external :: rn
  integer, intent(in) :: commblock
  integer :: spinbuf(0:N-1)
 
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
  
  if(commblock==1)then
    if(modulo(myrank,2)==0)then
      tosend=myrank+1
      call mpi_send(spin,N,mpi_int,tosend,49,mpi_comm_world,ierror)
      call mpi_send(info,1,infotype,tosend,50,mpi_comm_world,ierror)
      call mpi_recv(spinbuf,N,mpi_int,tosend,52,mpi_comm_world,stats,ierror)
      call mpi_recv(infobuf,1,infotype,tosend,51,mpi_comm_world,stats,ierror)
      info=infobuf
      E=info%Energy;spin=spinbuf
    else
      torecv=myrank-1
      call mpi_recv(spinbuf,N,mpi_int,torecv,49,mpi_comm_world,stats,ierror)
      call mpi_recv(infobuf,1,infotype,torecv,50,mpi_comm_world,stats,ierror)
      if(rn()<exp((E-infobuf%Energy)*(1/T - 1/infobuf%Temperature)))then
        call mpi_send(spin,N,mpi_int,torecv,52,mpi_comm_world,ierror)
        call mpi_send(info,1,infotype,torecv,51,mpi_comm_world,ierror)
        info=infobuf
        E=info%Energy;spin=spinbuf
      else
        call mpi_send(spinbuf,N,mpi_int,torecv,52,mpi_comm_world,ierror)
        call mpi_send(infobuf,1,infotype,torecv,51,mpi_comm_world,ierror)
      end if
    end if
  else
    if(modulo(myrank,2)/=0)then
      if(myrank/= numtasks-1)then
        tosend=myrank+1
        call mpi_send(spin,N,mpi_int,tosend,69,mpi_comm_world,ierror)
        call mpi_send(info,1,infotype,tosend,68,mpi_comm_world,ierror)
        call mpi_recv(spinbuf,N,mpi_int,tosend,72,mpi_comm_world,stats,ierror)
        call mpi_recv(infobuf,1,infotype,tosend,73,mpi_comm_world,stats,ierror)
        info=infobuf
        E=info%Energy;spin=spinbuf
      end if
    else
      if(myrank/=0)then
        torecv=myrank-1
        call mpi_recv(spinbuf,N,mpi_int,torecv,69,mpi_comm_world,stats,ierror)
        call mpi_recv(infobuf,1,infotype,torecv,68,mpi_comm_world,stats,ierror)
        if(rn()<exp((E-infobuf%Energy)*(1/T - 1/infobuf%Temperature)))then
          call mpi_send(spin,N,mpi_int,torecv,72,mpi_comm_world,ierror)
          call mpi_send(info,1,infotype,torecv,73,mpi_comm_world,ierror)
          info=infobuf
          E=info%Energy;spin=spinbuf
        else
          call mpi_send(spinbuf,N,mpi_int,torecv,72,mpi_comm_world,ierror)
          call mpi_send(infobuf,1,infotype,torecv,73,mpi_comm_world,ierror)
        end if
      end if
    end if
  end if
end subroutine swapspin


