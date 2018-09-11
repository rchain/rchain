import pytest
import inspect

# def wrapper(request, a,b,c):
#     import inspect
#     fixture_values = dict((f,request.getfuncargvalue(f.__name__)) for f in [a,b])
#     def get_value(p):
#         if p in fixture_values:
#             return request.getfuncargvalue(f.__name__)
#         else:
#             return p
#     parameter_values = [get_value(param_name) for param_name in [a,b,c]]
#     return fn(*param_values)

def make_wrapper(fn, fixtures):
    parameter_names = inspect.signature(fn).parameters.keys()
    parameter_list = ",".join(p for p in parameter_names if p != 'request')
    parameter_name_list = ",".join(f"('{p}', {p})" for p  in parameter_names)
    namespace = {"fn":fn, "fixtures":fixtures}

    wrapper_code = f"""
def wrapper(request, {parameter_list}):
    # import logging
    # logging.info("fixtures:" + str(fixtures))
    def get_value(p_name, p_value):
        if p_name in fixtures:
            v = request.getfuncargvalue(p_value.__name__)
            # logging.info("Get_value from fixtures: " + p_name + " : " + str(v))
            return v
        else:
            # logging.info("Get_value returns the object: " + p_name + " : " + str(p_value))
            return p_value
    param_values = [get_value(p_name, p_value) for p_name, p_value in [{parameter_name_list}]]
    return fn(*param_values)
"""

    exec(wrapper_code, locals(), namespace)
    return namespace["wrapper"]

class parametrize:
    @staticmethod
    def fixed(arg_names, arg_values):
        def decorator(fn):
            fixtures = arg_names.split(',')
            wrapper = make_wrapper(fn, fixtures)
            return pytest.mark.parametrize(arg_names, arg_values)(wrapper)
        return decorator

    @staticmethod
    def cartesian(**kwargs):
        def decorator(fn):
            fixtures = kwargs.keys()

            result_fn = make_wrapper(fn, fixtures)
            for arg in kwargs.keys():
                result_fn = pytest.mark.parametrize(arg, kwargs[arg])(result_fn)
            return result_fn
        return decorator