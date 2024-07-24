# Contributing to `frab`

Thank you for considering contributing to `frab`!  I appreciate your
interest in making this project better.

## Code of conduct

Please read and adhere to the [Code of Conduct](CODE_OF_CONDUCT.md) to
maintain a safe, welcoming, and inclusive environment.

## Types of contributions

Various forms of contributions are welcome:

- **Bug Reports**: Feel free to report any bugs you encounter.
- **Documentation**: Typos, clarity issues, or missing guides: your
  help is welcome here.
- **Feature Discussions/Requests**: Got an idea? Open an issue to
  discuss its potential.
- **Code Contributions**: All code contributions are welcome.

## Using co-authored-by in git commits

The use of [co-authored
commits](https://docs.github.com/en/github/committing-changes-to-your-project/creating-a-commit-with-multiple-authors)
is encouraged for collaborative efforts.  This helps in giving credit
to all contributors for their work.

```markdown
Co-authored-by: name <name@example.com>
Co-authored-by: another-name <another-name@example.com>
```

## Development

Your contributions make this project better for everyone.  Thank you
for participating!

Also don't forget to recreate the `readme` file:
```{r eval=FALSE}
devtools::build_readme()
```

#### Tests

Tests and checks are run on the CI, however locally one can use:

```bash
Rscript -e 'devtools::test()'
```

#### Documentation

Ideally each change should be documented.  Major changes should be `vignettes`.

