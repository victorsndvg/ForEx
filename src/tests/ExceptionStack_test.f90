program ExceptionStackNode_test

USE BaseException
USE ExceptionStack

implicit none

    print*, '1.- Open try frame'
    call TheExceptionStack%OpenFrame()
    do while(.true.)
        select type (P=>Exception(Code=999, Message='This is a generic exception'));
            class is (Exception);
                print*, '2.- Exception throwed'
                call TheExceptionStack%Push(P, __FILE__, __LINE__)
                exit;
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    if(.not. TheExceptionStackIterator%hasCatched()) call TheExceptionStackIterator%Init(TheExceptionStack)
    do while(.not. TheExceptionStackIterator%HasFinished() .and. .not. TheExceptionStackIterator%hasCatched())
        select type (E=>TheExceptionStackIterator%Get())
            class is(Exception)
                print*, '3.- Exception catched'
                call E%Catch()
                call TheExceptionStackIterator%Catch()
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    do while(.true.)
        select type (P=>TheExceptionStack%Top())
            class is (Exception)
                print*, '4.- Finally'
            class DEFAULT
                call TheExceptionStackIterator%Next()
                cycle
        end select
        exit
    enddo
    if(TheExceptionStackIterator%hasCatched()) call TheExceptionStackIterator%Del()
    call TheExceptionStackIterator%Free()
    call TheExceptionStack%CloseFrame()
    print*, '5.- Close try frame'


end program ExceptionStackNode_test
