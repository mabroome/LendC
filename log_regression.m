%% Predict loans that will default
%
%  
%  The following functions are used in this program:
%
%     sigmoid.m
%     costFunction.m
%     predict.m
%     costFunctionReg.m
%
% 
%

%% Initialization
clear ; close all; clc

%% Load Data
%   Load "learn_n.txt" which is the normalized loan data (n indicates normalized)
%   Load "learn_y_default" which identified loans that defaulted 1 or fully paid 0

All_Loan_data = load('learn_n.txt');
All_Y_defaults = load('learn_y_default.txt');

fprintf('Loaded matrices %f\n');

% Reduce the size of the Loan Data for initial testing

% incrment=size(Loan_Data,1)/25

% For i = 1000:increment:size(Loan_Data,1)

i = 1000;

    Loan_data = All_Loan_data(1:i,:);
    Y_defaults = All_Y_defaults(1:i,:);
    y = Y_defaults;
    m = size(Loan_data,1);
    n = size(Loan_data,2);



    % Initialize fitting parameters
    initial_theta = zeros(n, 1);
    
    % Initialize Y prediction 
    p = zeros(m,1);

    % Set regularization parameter lambda to 1
    lambda = 1;

    % Compute and display initial cost and gradient for regularized logistic
    % regression

    [cost, grad] = costFunctionReg(initial_theta, Loan_data, Y_defaults, lambda);

    fprintf('Cost at initial theta (zeros): %f\n', cost);

    fprintf('\nProgram paused. Press enter to continue.\n');


    % Set Options
    options = optimset('GradObj', 'on', 'MaxIter', 10);

    % Optimize
    [theta, J, exit_flag] = ...
        fminunc(@(t)(costFunctionReg(t, Loan_data, Y_defaults, lambda)), initial_theta, options);


    % Compute accuracy on our training set
   
   p = predict(theta,Loan_data);

    % fprintf('Train Accuracy: %f\n', p);
   fprintf('Train Accuracy: %f\n', mean((p == Y_defaults) * 100);
    % fprintf('Number of Samples',i)
% end


