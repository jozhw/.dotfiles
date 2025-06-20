// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

// https://astro.build/config
export default defineConfig({
	integrations: [
		starlight({
			title: 'Dotfiles',
			social: [{ icon: 'github', label: 'GitHub', href: 'https://github.com/jozhw/.dotfiles' }],
			sidebar: [{
					label: 'Overview',
					autogenerate: { directory: 'overview' },
				},
				{
					label: 'Development',
					autogenerate: { directory: 'development' },
				},
				{
					label: 'Configurations',
					autogenerate: { directory: 'configurations' },
				},
				{
					label: 'Atzlan',
                                        autogenerate: { directory: 'atzlan' },
				},

				{
					label: 'Garage',
					autogenerate: { directory: 'garage' },
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				},
			],
		}),
	],
});
