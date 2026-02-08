/**
 * Lazuli UI Controls
 * Handles control bindings, effect management, and UI interactions
 */

// Global state
const state = {
    style: 'crystal',
    palette: 'sunset',
    seed: 42,
    freq: 1.0,
    invert: false,
    effects: []
};

// Effect definitions loaded from API
let effectDefinitions = [];

// DOM Elements
const elements = {
    styleSelect: document.getElementById('style-select'),
    styleDescription: document.getElementById('style-description'),
    paletteSelect: document.getElementById('palette-select'),
    palettePreview: document.getElementById('palette-preview'),
    seedInput: document.getElementById('seed-input'),
    randomSeedBtn: document.getElementById('random-seed'),
    freqSlider: document.getElementById('freq-slider'),
    freqValue: document.getElementById('freq-value'),
    invertCheck: document.getElementById('invert-check'),
    effectsList: document.getElementById('effects-list'),
    addEffectBtn: document.getElementById('add-effect'),
    menuToggle: document.getElementById('menu-toggle'),
    closeMenu: document.getElementById('close-menu'),
    controlsPanel: document.getElementById('controls-panel'),
    cliCommand: document.getElementById('cli-command'),
    copyCmdBtn: document.getElementById('copy-cmd'),
    loadingIndicator: document.getElementById('loading-indicator'),
    previewCanvas: document.getElementById('preview-canvas')
};

// Initialize controls
async function initControls() {
    // Load metadata from API
    await loadMetadata();
    
    // Bind event listeners
    bindEventListeners();
    
    // Update UI from state
    updateUIFromState();
}

// Load metadata from API
async function loadMetadata() {
    try {
        const [styles, palettes, effects] = await Promise.all([
            fetch('/api/styles').then(r => r.json()),
            fetch('/api/palettes').then(r => r.json()),
            fetch('/api/effects').then(r => r.json())
        ]);
        
        console.log('Loaded effects:', effects);
        populateStyleSelect(styles);
        populatePaletteSelect(palettes);
        effectDefinitions = effects;
        
        // Set initial values
        if (styles.length > 0) state.style = styles[0].name;
        if (palettes.length > 0) state.palette = palettes[0].name;
        
        updateUIFromState();
    } catch (error) {
        console.error('Failed to load metadata:', error);
        showToast('Failed to load settings', 'error');
    }
}

// Populate style dropdown
function populateStyleSelect(styles) {
    elements.styleSelect.innerHTML = styles.map(s => 
        `<option value="${s.styleName}">${s.styleName}</option>`
    ).join('');
}

// Populate palette dropdown
function populatePaletteSelect(palettes) {
    elements.paletteSelect.innerHTML = palettes.map(p => 
        `<option value="${p.paletteName}">${p.paletteName}</option>`
    ).join('');
}

// Bind event listeners
function bindEventListeners() {
    // Style select
    elements.styleSelect.addEventListener('change', (e) => {
        state.style = e.target.value;
        updateStyleDescription();
        requestPreview();
    });
    
    // Palette select
    elements.paletteSelect.addEventListener('change', (e) => {
        state.palette = e.target.value;
        updatePalettePreview();
        requestPreview();
    });
    
    // Seed input
    elements.seedInput.addEventListener('change', (e) => {
        const val = parseInt(e.target.value);
        if (!isNaN(val)) {
            state.seed = val;
            requestPreview();
        }
    });
    
    // Random seed button
    elements.randomSeedBtn.addEventListener('click', () => {
        state.seed = Math.floor(Math.random() * 1000000);
        elements.seedInput.value = state.seed;
        requestPreview();
    });
    
    // Frequency slider (update value on input, render on change)
    elements.freqSlider.addEventListener('input', (e) => {
        state.freq = parseFloat(e.target.value);
        elements.freqValue.textContent = state.freq.toFixed(1);
    });
    
    elements.freqSlider.addEventListener('change', () => {
        requestPreview();
    });
    
    // Invert checkbox
    elements.invertCheck.addEventListener('change', (e) => {
        state.invert = e.target.checked;
        requestPreview();
    });
    
    // Add effect button
    elements.addEffectBtn.addEventListener('click', addEffect);
    
    // Mobile menu toggle
    elements.menuToggle.addEventListener('click', () => {
        elements.controlsPanel.classList.add('open');
    });
    
    elements.closeMenu.addEventListener('click', () => {
        elements.controlsPanel.classList.remove('open');
    });
    
    // Copy command button
    elements.copyCmdBtn.addEventListener('click', copyCommand);
}

// Update UI from state
function updateUIFromState() {
    elements.styleSelect.value = state.style;
    elements.paletteSelect.value = state.palette;
    elements.seedInput.value = state.seed;
    elements.freqSlider.value = state.freq;
    elements.freqValue.textContent = state.freq.toFixed(1);
    elements.invertCheck.checked = state.invert;
    
    updateStyleDescription();
    updatePalettePreview();
    updateCliCommand();
}

// Update style description
function updateStyleDescription() {
    // Note: styles come from a different endpoint than effects
    // For now, we'll skip this or implement it properly later
    elements.styleDescription.textContent = '';
}

// Update palette preview
function updatePalettePreview() {
    // This would show color swatches based on the palette
    // For now, just a placeholder
    elements.palettePreview.innerHTML = '<div style="background: linear-gradient(90deg, #FF6B35, #F7931E, #FFD23F); height: 100%; width: 100%;"></div>';
}

// Update CLI command display
function updateCliCommand() {
    const parts = ['lazuli'];
    parts.push(`--style ${state.style}`);
    parts.push(`--palette ${state.palette}`);
    parts.push(`--seed ${state.seed}`);
    parts.push(`--freq ${state.freq}`);
    if (state.invert) parts.push('--invert');
    state.effects.forEach(e => parts.push(`--effect "${e}"`));
    parts.push('-o wallpaper.png');
    
    elements.cliCommand.textContent = parts.join(' ');
}

// Copy command to clipboard
function copyCommand() {
    navigator.clipboard.writeText(elements.cliCommand.textContent).then(() => {
        showToast('Command copied to clipboard!', 'success');
    }).catch(() => {
        showToast('Failed to copy command', 'error');
    });
}

// Add new effect
function addEffect() {
    const effectId = Date.now();
    const effectItem = document.createElement('div');
    effectItem.className = 'effect-item';
    effectItem.dataset.id = effectId;
    
    effectItem.innerHTML = `
        <div class="effect-header">
            <select class="effect-select">
                <option value="" disabled selected>Select effect...</option>
                ${effectDefinitions.map(e => `<option value="${e.effectName}">${e.effectName}</option>`).join('')}
            </select>
            <div class="effect-actions">
                <button class="btn-move-up" title="Move up">↑</button>
                <button class="btn-move-down" title="Move down">↓</button>
                <button class="btn-remove" title="Remove">×</button>
            </div>
        </div>
        <div class="effect-params"></div>
    `;
    
    // Bind effect select
    const select = effectItem.querySelector('.effect-select');
    select.addEventListener('change', (e) => {
        const effectName = e.target.value;
        const effectDef = effectDefinitions.find(ed => ed.effectName === effectName);
        if (effectDef) {
            renderEffectParams(effectItem, effectDef);
            updateEffectsList();
        }
    });
    
    // Bind action buttons
    effectItem.querySelector('.btn-remove').addEventListener('click', () => {
        effectItem.remove();
        updateEffectsList();
        requestPreview();
    });
    
    effectItem.querySelector('.btn-move-up').addEventListener('click', () => {
        const prev = effectItem.previousElementSibling;
        if (prev) {
            effectItem.parentNode.insertBefore(effectItem, prev);
            updateEffectsList();
            requestPreview();
        }
    });
    
    effectItem.querySelector('.btn-move-down').addEventListener('click', () => {
        const next = effectItem.nextElementSibling;
        if (next) {
            effectItem.parentNode.insertBefore(next, effectItem);
            updateEffectsList();
            requestPreview();
        }
    });
    
    elements.effectsList.appendChild(effectItem);
}

// Render effect parameters
function renderEffectParams(effectItem, effectDef) {
    const paramsContainer = effectItem.querySelector('.effect-params');
    
    if (!effectDef) {
        console.error('No effectDef provided to renderEffectParams');
        return;
    }
    
    // API returns effectParams, not params
    const params = effectDef.effectParams || [];
    
    console.log('Rendering params for', effectDef.effectName, ':', JSON.stringify(params));
    
    if (params.length === 0) {
        paramsContainer.innerHTML = '';
        return;
    }
    
    try {
        const html = params.map((param, idx) => {
            console.log('  Param', idx, ':', param);
            
            // API uses paramMin, paramMax, paramDefault, paramName
            const min = param.paramMin !== null && param.paramMin !== undefined ? param.paramMin : 0;
            const max = param.paramMax !== null && param.paramMax !== undefined ? param.paramMax : 100;
            const def = param.paramDefault !== null && param.paramDefault !== undefined 
                ? parseFloat(param.paramDefault) 
                : (min + max) / 2;
            
            console.log('  -> min:', min, 'max:', max, 'def:', def);
            
            return `
                <div class="effect-param" data-param="${param.paramName}">
                    <label>${param.paramName}</label>
                    <input type="range" min="${min}" max="${max}" step="0.1" value="${def}">
                    <span class="param-value">${def.toFixed(1)}</span>
                </div>
            `;
        }).join('');
        
        console.log('Generated HTML length:', html.length);
        paramsContainer.innerHTML = html;
        
        // Bind parameter inputs
        paramsContainer.querySelectorAll('input[type="range"]').forEach(input => {
            input.addEventListener('input', (e) => {
                e.target.nextElementSibling.textContent = parseFloat(e.target.value).toFixed(1);
            });
            input.addEventListener('change', () => {
                updateEffectsList();
                requestPreview();
            });
        });
    } catch (err) {
        console.error('Error rendering params:', err);
        paramsContainer.innerHTML = '<div style="color: red;">Error loading parameters</div>';
    }
}

// Update effects list from DOM
function updateEffectsList() {
    state.effects = Array.from(elements.effectsList.children).map(item => {
        const select = item.querySelector('.effect-select');
        const effectName = select.value;
        if (!effectName) return null;
        
        // Collect parameters (just values, in order)
        const params = Array.from(item.querySelectorAll('.effect-param')).map(paramEl => {
            return paramEl.querySelector('input').value;
        });
        
        return params.length > 0 ? `${effectName}:${params.join(':')}` : effectName;
    }).filter(Boolean);
    
    updateCliCommand();
}

// Show toast notification
function showToast(message, type = 'info') {
    const container = document.getElementById('toast-container');
    const toast = document.createElement('div');
    toast.className = `toast ${type}`;
    toast.textContent = message;
    
    container.appendChild(toast);
    
    setTimeout(() => {
        toast.style.animation = 'slideIn 0.3s ease reverse';
        setTimeout(() => toast.remove(), 300);
    }, 3000);
}

// Show/hide loading indicator
function setLoading(loading) {
    if (loading) {
        elements.loadingIndicator.classList.remove('hidden');
    } else {
        elements.loadingIndicator.classList.add('hidden');
    }
}

// Display rendered image
function displayImage(base64Data) {
    const img = new Image();
    img.onload = () => {
        const ctx = elements.previewCanvas.getContext('2d');
        ctx.clearRect(0, 0, elements.previewCanvas.width, elements.previewCanvas.height);
        ctx.drawImage(img, 0, 0);
    };
    img.src = `data:image/png;base64,${base64Data}`;
}
