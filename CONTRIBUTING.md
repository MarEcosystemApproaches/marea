# Contributing to marea

## Development Workflow

### Setting Up Your Development Environment

1. Fork and clone the repository
2. Create a development branch: `git checkout -b dev-yourname`
3. Install development dependencies: `devtools::install_deps(dependencies = TRUE)`

### Making Changes

1. Ensure your branch is up to date with main
2. Make changes and add tests
3. Run checks locally: `devtools::check()`
4. Update documentation: `devtools::document()`
5. Update NEWS.md with user-facing changes

### Submitting Changes

1. Push to your development branch
2. Ensure GitHub Actions pass
3. Merge into main when ready

### Coding Standards

- Use `snake_case` for functions and variables
- Document all exported functions
- Include examples in documentation
- Add tests for new functionality
- Maintain >80% code coverage

### Git Branching Strategy

- Use `main` for stable releases
- Use `dev-*` branches for development
- Use descriptive branch names for features or fixes
- Regularly merge `main` into your development branch to keep it up to date
- Use pull requests for code reviews and merging
