const fs = require('fs');
const path = process.argv[2] || 'ST4_Aks_Ciz.lsp';
const content = fs.readFileSync(path, 'utf8');

let depth = 0;
let line = 1, col = 0;
let inComment = false, inString = false, escapeNext = false;
let lastOpenLine = null, lastOpenCol = null;

for (let i = 0; i < content.length; i++) {
  const c = content[i];
  if (c === '\n') {
    line++; col = 0;
    inComment = false;
    continue;
  }
  col++;

  if (escapeNext) {
    escapeNext = false;
    continue;
  }
  if (inComment) continue;
  if (inString) {
    if (c === '\\') escapeNext = true;
    else if (c === '"') inString = false;
    continue;
  }
  if (c === ';') {
    inComment = true;
    continue;
  }
  if (c === '"') {
    inString = true;
    continue;
  }
  if (c === '(') {
    depth++;
    lastOpenLine = line;
    lastOpenCol = col;
  } else if (c === ')') {
    depth--;
    if (depth < 0) {
      console.log(`Extra ) at line ${line}, column ${col}`);
      process.exit(0);
    }
  }
}

if (depth > 0) {
  console.log(`Extra ( - ${depth} unclosed. Last ( opened at line ${lastOpenLine}, column ${lastOpenCol}.`);
  console.log(`Missing ) should close the form that starts at line ${lastOpenLine}.`);
}
