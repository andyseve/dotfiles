self: super:

{
  all-hies = import <all-hie> {};
  unstable = import <unstable> { config = super.config; };
}
