const link = document.querySelector('.href-git-edit-button');
if (link) {
  // console.log(link);
  var filename = link.getAttribute('href');
  const fileList = [
    'CITE-THIS-LIBRARY.md',
    'CODINGSTYLE.md',
    'CONTRIBUTING.md',
    'CONTRIBUTORS.md',
    'DESIGN-PRINCIPLES.md',
    'FILE-CONVENTIONS.md',
    'GRANT-ACKNOWLEDGEMENTS.md',
    'HOME.md',
    'HOWTO-INSTALL.md',
    'LICENSE.md',
    'MAINTAINERS.md',
    'README.md',
    'STATEMENT-OF-INCLUSION.md',
    'SUMMARY.md',
    'USERS.md',
    'index.md',
  ];
  if (!fileList.includes(filename)) {
    const dotIndex = filename.lastIndexOf('.');
    let name = filename.slice(0, dotIndex);
    // const extension = filename.slice(dotIndex + 1);
    filename = 'src/' + name.replace(/\./g, '/') + '.lagda.md';
  } else if (filename === 'index.md') {
    filename = 'HOME.md';
  }
  const prefixedHref =
    'https://github.com/UniMath/agda-unimath/edit/master/' + filename;
  link.setAttribute('href', prefixedHref);
} else {
  console.error('Could not find a link with the class href-git-edit-button');
}
