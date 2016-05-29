# Kredite hängen 

upper.p = 1000
gamma   = 0.5
delta.p[1]  = 10

delta.p[t] = gamma*g.p[t-1] + alpha*loans



delta.p  = alpha*d + beta*Edelta.p
Edelta.p = delta.p[t-1]
delta.d = delta*Loans
Loans = delta.p 

demand = 