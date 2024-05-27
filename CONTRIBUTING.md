# Contributing to Bend 
Thank you for considering contributing to Bend!

## How to Contribute
### Reporting bugs
1. **Check for existing issues:** Before you create a new issue, please do a search in [our issues](https://github.com/HigherOrderCO/Bend/issues) to see if the issue or feature request has already been filed.
2. **Create a new issue:** If you find no issue or your issue differs, [create a new issue](https://github.com/HigherOrderCO/Bend/issues/new?template=bug_report.yml) and provide detailed information, including steps to reproduce the problem.

### Suggesting Enhancements
1. **Check for existing suggestions:** Before suggesting a new feature, please check if it's already been suggested in [our issues](https://github.com/HigherOrderCO/Bend/issues).
2. **Create a new suggestion:** If you find no similar suggestion, [create a new suggestion issue](https://github.com/HigherOrderCO/Bend/issues/new?template=feature_request.md) and provide detailed information about the enhancement and why it would be useful.

### Checking HVM Issues
Since Bend depends on HVM, some issues might be relateds to HVM rather than Bend itself. Please also check the [HVM issues page](https://github.com/HigherOrderCO/HVM/issues) to see if your issue has already been reported there.

### Submitting Changes
1. **Fork the repository:** Create your own fork of the repository on GitHub.
2. **Create a new branch:** Make your changes in a new branch in your forked repository.
3. **Run formatting and linting checks:** Before submitting your changes, ensure your code is properly formated asnd linted:
  - Run `cargo fmt` to format your code.
  - Run `cargo clippy` to lint your code.
4. **Run tests:** Ensure all tests pass and update any test results:
  - Run `cargo insta test` to run the tests.
  - Run `cargo insta review` to save any changes to the test results.
5. **Submit a pull request:** Once your changes are ready, submit a pull request from your branch to the `main` branch of the Bend Repository


We appreciate every contribution!