asc = function(x, y0, y1, s0, s1)
{
  xx = s0 + x * (s1 - s0)
  h0 = exp(s0) / (1 + exp(s0))
  h1 = exp(s1) / (1 + exp(s1))
  h = (exp(xx) / (1 + exp(xx)) - h0) / (h1 - h0)
  y0 + (y1 - y0) * h
}


