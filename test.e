// fn tests

start         add x x step
              call add5 retaddr
              add x x step
              be wloop 0 0

add5          add x x five
              ret retaddr

five 5
x 0
retaddr 0

// array tests

wloop         cpta ptr arr ptr
              add ptr ptr step
              bne wloopi ptr econd

rloop         cpfa store arr ptr
              cpta store arr eptr
              add eptr eptr step
              sub ptr ptr step
              bne rloop eptr econd

end           halt

step 1
econd 10
store 0
ptr 0
eptr 0
arr 0
