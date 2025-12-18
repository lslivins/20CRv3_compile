program testlog
logical*1 a1,a2
logical b1,b2
integer i1
i1 = 1
a1 = i1
a2 = .false.
b1 = a1
b2 = a2
i1 = a1
print  *,a1,a2,a1+a2,i1
print *,b1,b2
end
