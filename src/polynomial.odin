package bilang

Polynomial :: distinct []f64

@require_results
polynomial_degree :: proc (p: Polynomial) -> int {
	return len(p) - 1
}

@require_results
polynomial_from_atom :: proc (atom: Atom) -> (p: Polynomial, ok: bool)
{
	add := atom.(Atom_Add) or_return

	coefficients: [4]f64 // only allow 4 coefficients for now
	filled      : [4]bool
	size        : int

	if len(add.addends) > len(coefficients) {
		return
	}

	for addend in add.addends {
		i: int
		coefficient: f64
		#partial switch v in addend {
		case Atom_Num:
			i = 0
			coefficient = v.num/v.den
		case Atom_Var: // TODO: check if vars are the same
			i = 1
			coefficient = v.f.num/v.f.den
		case Atom_Mul:
			if len(v.factors) != 2 do return
			num   := v.factors[0].(Atom_Num) or_return
			pow   := v.factors[1].(Atom_Pow) or_return
			_      = pow.lhs.(Atom_Var) or_return // TODO: check if vars are the same
			exp   := pow.rhs.(Atom_Num) or_return
			exp_f := exp.num/exp.den
			i      = int(exp_f)
			if f64(i) != exp_f do return
			coefficient = num.num/num.den
		case Atom_Pow:
			// only integers allowed,
			// might need to deal with computational round-off later
			// but false negatives are better than false positives here
			_      = v.lhs.(Atom_Var) or_return // TODO: check if vars are the same
			exp   := v.rhs.(Atom_Num) or_return
			float := exp.num/exp.den
			i      = int(float)
			if f64(i) != float do return
			coefficient = 1
		case:
			return
		}

		if i >= len(coefficients) || filled[i] {
			return
		}

		filled[i] = true
		coefficients[i] = coefficient
		size = max(size, i+1)
	}

	for is_filled in filled[:size] {
		if !is_filled do return
	}

	return Polynomial(coefficients[:size]), true
}

/*
// Define the function f(x) = x^3 + x^2 + 12x + 8
double f(double x) {
    return x * x * x + x * x + 12 * x + 8;
}

// Define the derivative f'(x) = 3x^2 + 2x + 12
double f_prime(double x) {
    return 3 * x * x + 2 * x + 12;
}

// Bisection method to find an initial guess
double bisection(double a, double b, double tolerance) {
    double c;
    while ((b - a) >= tolerance) {
        c = (a + b) / 2; // Midpoint
        if (f(c) == 0.0) {
            break; // c is a root
        } else if (f(c) * f(a) < 0) {
            b = c;
        } else {
            a = c;
        }
    }
    return c;
}

// Newton-Raphson method
double newton_raphson(double initial_guess, double tolerance, int max_iter) {
    double x0 = initial_guess;
    double x1;
    int iter = 0;
    
    while (iter < max_iter) {
        x1 = x0 - f(x0) / f_prime(x0);
        
        if (fabs(x1 - x0) < tolerance) {
            break;
        }
        
        x0 = x1;
        iter++;
    }
    
    if (iter == max_iter) {
        printf("The method did not converge within the maximum number of iterations.\n");
        return x0; // Return the best approximation found
    }
    
    return x1;
}

int main() {
    double a = -2.0;  // Interval start
    double b = 0.0;   // Interval end
    double tolerance = 1e-6;  // Desired precision
    int max_iter = 1000;  // Maximum number of iterations
    
    // Find initial guess using Bisection method
    double initial_guess = bisection(a, b, tolerance);
    printf("Initial guess from Bisection method: %lf\n", initial_guess);
    
    // Find root using Newton-Raphson method
    double root = newton_raphson(initial_guess, tolerance, max_iter);
    printf("Root found: %lf\n", root);
    
    return 0;
}
*/
