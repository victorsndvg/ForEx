program ExceptionStackNode_test

USE BaseException
USE ExceptionStack

implicit none

BLOCK
    class(Exception), allocatable :: NullException
    allocate(NullException)
    print*, '1.- Open try frame'
    call TheExceptionStack%OpenFrame()
    do while(.true.)
        select type (TheException=>Exception(Code=999, Message='This is a generic exception'));
            class is (Exception);
                associate(TheException=>TheException)
                    if(.true.) then
                        print*, '2.- Exception throwed'
                        call TheExceptionStack%Push(TheException, __FILE__, __LINE__)
                        exit;
                    endif
                end associate
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    call TheExceptionStackIterator%Init(TheExceptionStack)
    do while(.not. TheExceptionStackIterator%HasFinished())
        select type (TheException=>TheExceptionStackIterator%Get())
            class is(Exception)
                print*, '3.- Exception catched'
                call TheException%Catch()
                if(allocated(NullException)) deallocate(NullException)
                allocate(NullException, Mold=TheException)
                call NullException%Clone(TheException)
                call TheExceptionStack%Clean()
                call TheExceptionStackIterator%Free()
                associate(Ex=>NullException)
                end associate
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    do while(.true.)
        select type (TheException=>NullException)
            class is (Exception)
                print*, '4.- Finally'
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    call TheExceptionStack%CloseFrame()
    print*, '5.- Close try frame'
ENDBLOCK


end program ExceptionStackNode_test
