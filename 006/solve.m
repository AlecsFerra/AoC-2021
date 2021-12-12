input = [0,1,1,2,1,0,0,0,0]';

tick = [ 0 0 0 0 0 0 1 0 1 
         eye(8) zeros(8, 1) ]';

#one = tick ^ 80;
#two = tick ^ 256;
t = tick ^ 100000;

#sprintf("80 days: %d", sum(one * input))
#sprintf("256 days: %d", sum(two * input))
sprintf("100000 days: %d", sum(t * input))
