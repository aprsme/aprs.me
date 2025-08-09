// Combined Chart.js and date-fns adapter bundle
// This bundles Chart.js first, then the date adapter that depends on it

// Load Chart.js directly (not as a module)
const chartScript = document.createElement('script');
chartScript.textContent = `
${require('fs').readFileSync(__dirname + '/chart-minimal.js', 'utf8')}
`;
document.head.appendChild(chartScript);

// Then load the date adapter after a brief delay
setTimeout(() => {
  const adapterScript = document.createElement('script');
  adapterScript.textContent = `
${require('fs').readFileSync(__dirname + '/date-adapter.js', 'utf8')}
  `;
  document.head.appendChild(adapterScript);
}, 10);