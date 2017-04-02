function g = sigmoid(z)
%SIGMOID Compute sigmoid function
%   g = SIGMOID(z) computes the sigmoid of z.

% You need to return the following variables correctly 
g = zeros(size(z));

% divones = ones(m,1);

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the sigmoid of each value of z (z can be a matrix,
%               vector or scalar).


% Calculate the neg exp of Matrix Z (16); add 1 (17) and then calculate 1/ t2
t1 = exp(-z);
t2 = 1 + t1;

g = ones(size(z)) ./t2;


% =============================================================

end
